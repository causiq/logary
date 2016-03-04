module Logary.Targets.InfluxDb

open Hopac
open Hopac.Infixes
open HttpFs.Client
open Logary
open Logary.Target
open Logary.Internals
open System

module Serialisation =

  let serialiseTimestamp (ts : EpochNanoSeconds) =
    ts.ToString(Culture.invariant)

  let escapeString (s : string) =
    s.Replace(" ", @"\ ")
     .Replace(",", @"\,")

  let serialiseStringTag (s : string) =
    escapeString s

  let serialiseStringValue (s : string) =
    "\"" + escapeString s + "\""

  let serialisePointName (pn : PointName) =
    escapeString (pn.ToString())

  let printTags = function
    | [] ->
      ""

    | xs ->
      // TO CONSIDER: assumes values are escaped
      let joined =
        xs |> List.map (fun (x, y) -> sprintf "%s=%s" x y)
           |> String.concat ","
      "," + joined

  let printValues = function
    | [] ->
      "value=0"

    | xs ->
      // TO CONSIDER: assumes values are escaped
      xs |> List.map (fun (x, y) -> sprintf "%s=%s" x y)
         |> String.concat ","

  let serialiseMessage (message : Message) : string =
    let mapExtract fExtractValue =
      Map.map (fun k -> fExtractValue >> Option.orDefault "")
      >> Seq.filter (fun (KeyValue (k, v)) -> v <> String.Empty)
      >> Seq.map (fun (KeyValue (k, v)) -> k, v)
      >> Seq.sortBy fst
      >> List.ofSeq

    let rec simpleValue
      isTag
      (kvs : (string * string) list)
      (values : (string * string) list) = function
      | Float v ->
        kvs, ("value", v.ToString(Culture.invariant)) :: values

      | Int64 v ->
        kvs, ("value", v.ToString(Culture.invariant) + "i") :: values

      | String s ->
        let fser = if isTag then serialiseStringTag else serialiseStringValue
        kvs, ("value", fser s) :: values

      | Bool true ->
        kvs, ("value", "true") :: values

      | Bool false ->
        kvs, ("value", "false") :: values

      | BigInt v ->
        if v > bigint Int64.MaxValue then
          let str = Int64.MaxValue.ToString(Culture.invariant) + "i"
          kvs, ("value", str) :: values
        elif v < bigint Int64.MinValue then
          let str = Int64.MinValue.ToString(Culture.invariant) + "i"
          kvs, ("value", str) :: values
        else
          let str = v.ToString(Culture.invariant) + "i"
          kvs, ("value", str) :: values

      | Binary (bs, ct) ->
        ("value_ct", ct) :: kvs,
        ("value", Convert.ToBase64String(bs)) :: values

      | Fraction (n, d) ->
        kvs,
        ("value", (n / d).ToString(Culture.invariant)) :: values

      | Object _ as o ->
        complexValue kvs values o

      | Array _ as a ->
        complexValue kvs values a

    and extractSimple isTag v =
      match simpleValue isTag [] [] v with
      | _, (_, str) :: _ -> Some str
      | _ -> None

    and complexValue kvs values = function
      | Object mapKvs ->
        let newValues = mapKvs |> mapExtract (extractSimple false)
        kvs, newValues @ values

      | Array arr ->
        let rec array i acc = function
          | [] ->
            acc

          | h :: tail ->
            match extractSimple false h with
            | Some str ->
              array (i + 1) ((sprintf "arr_%i" i, str) :: acc) tail
            | None ->
              array i acc tail

        kvs, array 0 [] arr

      | x ->
        failwithf "'%A' is not a complex value" x

    let outer context =
      let context' =
        context |> mapExtract (extractSimple true)

      function
      | Gauge (value, Scalar)
      | Derived (value, Scalar) ->
        simpleValue false context' [] value

      | Gauge (value, units)
      | Derived (value, units) ->
        let kvs, values = simpleValue false context' [] value
        ("unit", Units.symbol units) :: kvs,
        values

      | Event templ ->
        // TODO: provide hash
        simpleValue false context' [] (Value.Int64 1L)

    let kvs, values = outer message.context message.value

    sprintf "%O%s %s %i"
            (serialisePointName message.name)
            (printTags kvs)
            (printValues values)
            message.timestamp

type Consistency =
  /// the data must be written to disk by at least 1 valid node
  | One
  /// the data must be written to disk by (N/2 + 1) valid nodes (N is the replication factor for the target retention policy)
  | Quorum
  ///  the data must be written to disk by all valid nodes
  | All
  /// a write is confirmed if hinted-handoff is successful, even if all target nodes report a write failure In this context, “valid node” means a node that hosts a copy of the shard containing the series being written to. In a clustered system, the replication factor should equal the number of valid nodes.
  | Any

type InfluxDbConf =
    /// the write endpoint to send the values to
  { endpoint  : Uri
    /// REQUIRED - sets the target database for the write
    db        : string
    /// if authentication is enabled, you must authenticate as a user with write permissions to the target database
    user      : string option
    /// if authentication is enabled, you must authenticate as a user with write permissions to the target database
    password  : string option
    /// set the number of nodes that must confirm the write. If the requirement is not met the return value will be partial write if some points in the batch fail, or write failure if all points in the batch fail.
    consistency : Consistency
    /// sets the target retention policy for the write. If not present the default retention policy is used
    retention : string option }

  static member create(ep, db, ?user, ?password, ?consistency, ?retention) =
    { endpoint    = ep
      db          = db
      user        = defaultArg user None
      password    = defaultArg password None
      consistency = defaultArg consistency Quorum
      retention   = defaultArg retention None }

let empty =
  { endpoint    = Uri "http://127.0.0.1/write"
    db          = "SET_ME"
    user        = None
    password    = None
    consistency = Quorum
    retention   = None }

// When creating a new target this module gives the barebones
// for the approach you may need to take.
module internal Impl =

  let loop (conf : InfluxDbConf) (ri : RuntimeInfo)
           (requests : BoundedMb<_>)
           (shutdown : Ch<_>) =
    let rec loop () : Job<unit> =
      Alt.choose [
        shutdown ^=> fun ack ->
          ack *<= () :> Job<_>

        // 'When there is a request' call this function
        BoundedMb.take requests ^=> function
          | Log (msg, ack) ->
            let body = Serialisation.serialiseMessage msg
            let req =
              createRequest Post conf.endpoint
              |> withQueryStringItem "db" conf.db
              |> withBody (BodyString body)

            job {
              use! resp = getResponse req
              if resp.StatusCode <> 200 then failwithf "got response code %i" resp.StatusCode
              do! ack *<= ()
              return! loop ()
            }
          
          | Flush (ackCh, nack) ->
            job {
              do! Ch.give ackCh () <|> nack
              return! loop ()
            }
      ] :> Job<_>

    loop ()

/// Create a new Noop target
let create conf = TargetUtils.stdNamedTarget (Impl.loop conf)

/// Use with LogaryFactory.New( s => s.Target<Noop.Builder>() )
type Builder(conf, callParent : FactoryApi.ParentCallback<Builder>) =
  // TODO: expose for C# peeps
  member x.WriteEndpoint(writeEndpoint : Uri) =
    ! (callParent <| Builder({ conf with endpoint = writeEndpoint }, callParent))

  new(callParent : FactoryApi.ParentCallback<_>) =
    Builder(empty, callParent)

  interface Logary.Target.FactoryApi.SpecificTargetConf with
    member x.Build name = create conf name
