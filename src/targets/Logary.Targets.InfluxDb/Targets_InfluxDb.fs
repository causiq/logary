module Logary.Targets.InfluxDb

open Hopac
open Hopac.Infixes
open Hopac.Extensions
open HttpFs.Client
open Logary
open Logary.Target
open Logary.Internals
open System

module Serialisation =

  let serialiseTimestamp (ts : EpochNanoSeconds) =
    ts.ToString(Culture.invariant)

  let escapeString (s : string) =
    s
     .Replace(",", @"\,")
     .Replace(" ", "\ ")
     .Replace("=", "\=")

  let escapeStringValue (s : string) =
    s
     .Replace("\"", "\\\"")

  let serialiseStringTag (s : string) =
    escapeString s

  let serialiseStringValue (s : string) =
    "\"" + escapeStringValue s + "\""

  let serialisePointName (pn : PointName) =
    escapeString (pn.ToString())

  let printTags = function
    | [] ->
      ""

    | xs ->
      // TO CONSIDER: assumes values are escaped
      let joined =
        xs |> List.map (fun (x, y) -> sprintf "%s=%s" (escapeString x) y)
           |> String.concat ","
      "," + joined

  let printValues = function
    | [] ->
      "value=0"

    | xs ->
      // TO CONSIDER: assumes values are escaped
      xs |> List.map (fun (x, y) -> sprintf "%s=%s" (escapeString x) y)
         |> String.concat ","

  let serialiseMessage (message : Message) : string =
    let mapExtract fExtractValue =
      Map.map (fun k -> fExtractValue >> Option.orDefault "")
      >> Seq.filter (fun (KeyValue (k, v)) -> v <> String.Empty)
      >> Seq.map (fun (KeyValue (k, v)) -> k, v)
      >> Seq.sortBy fst
      >> List.ofSeq

    let rec getValues
      (valueKey : string)
      isTag
      (kvs : (string * string) list)
      (values : (string * string) list)  = function
      | Float v ->
        kvs, (valueKey, v.ToString(Culture.invariant)) :: values

      | Int64 v ->
        kvs, (valueKey, v.ToString(Culture.invariant) + "i") :: values

      | String s ->
        let fser = if isTag then serialiseStringTag else serialiseStringValue
        kvs, (valueKey, fser s) :: values

      | Bool true ->
        kvs, (valueKey, "true") :: values

      | Bool false ->
        kvs, (valueKey, "false") :: values

      | BigInt v ->
        if v > bigint Int64.MaxValue then
          let str = Int64.MaxValue.ToString(Culture.invariant) + "i"
          kvs, (valueKey, str) :: values
        elif v < bigint Int64.MinValue then
          let str = Int64.MinValue.ToString(Culture.invariant) + "i"
          kvs, (valueKey, str) :: values
        else
          let str = v.ToString(Culture.invariant) + "i"
          kvs, (valueKey, str) :: values

      | Binary (bs, ct) ->
        ("value_ct", ct) :: kvs,
        (valueKey, Convert.ToBase64String(bs)) :: values

      | Fraction (n, d) ->
        kvs,
        (valueKey, (n / d).ToString(Culture.invariant)) :: values

      | Object _ as o ->
        complexValue valueKey kvs values o

      | Array _ as a ->
        complexValue valueKey kvs values a

    and extractSimple valueKey isTag v =
      match getValues valueKey isTag [] [] v with
      | _, (_, str) :: _ -> Some str
      | _ -> None

    and complexValue valueKey kvs values = function
      | Object mapKvs ->
        let newValues = mapKvs |> mapExtract (extractSimple valueKey false)
        kvs, newValues @ values

      | Array arr ->
        let rec array i acc = function
          | [] ->
            acc

          | h :: tail ->
            match extractSimple valueKey false h with
            | Some str ->
              array (i + 1) ((sprintf "arr_%i" i, str) :: acc) tail
            | None ->
              array i acc tail

        kvs, array 0 [] arr

      | x ->
        failwithf "'%A' is not a complex value" x

    let simpleValue = getValues "value"

    let outer context =
      let context' =
        context |> mapExtract (extractSimple "value" true)

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
        let kvs, values = simpleValue false context' [] (Value.Int64 1L)
        ("Event", serialiseStringTag templ) :: kvs, values

    let fieldValues (fields : Map<PointName, Field>)  =
      Map.toSeq fields
      |> Seq.map (fun (key, (Field (value, _))) -> PointName.format key, value)
      |> Map.ofSeq
      |> Value.Object
      |> getValues "value" false [] []

    let fieldkvs, fieldvalues = fieldValues message.fields

    let kvs, values = outer message.context message.value

    let kvs = List.concat [fieldkvs; kvs]
    let values = List.concat [fieldvalues; values]

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
    username  : string option
    /// if authentication is enabled, you must authenticate as a user with write permissions to the target database
    password  : string option
    /// set the number of nodes that must confirm the write. If the requirement is not met the return value will be partial write if some points in the batch fail, or write failure if all points in the batch fail.
    consistency : Consistency
    /// sets the target retention policy for the write. If not present the default retention policy is used
    retention : string option
    /// Sets how many measurements should be batched together if new measurements are produced faster than we can write them one by one. Default is 100. 
    batchSize : uint16 }

  static member create(ep, db, ?user, ?password, ?consistency, ?retention, ?batchSize) =
    { endpoint    = ep
      db          = db
      username    = defaultArg user None
      password    = defaultArg password None
      consistency = defaultArg consistency Quorum
      retention   = defaultArg retention None
      batchSize   = defaultArg batchSize 100us }

let empty =
  { endpoint    = Uri "http://127.0.0.1:8086/write"
    db          = "logary"
    username    = None
    password    = None
    consistency = Quorum
    retention   = None
    batchSize   = 100us }

// When creating a new target this module gives the barebones
// for the approach you may need to take.
module internal Impl =

  let reqestAckJobCreator request =
    match request with
    | Log (msg, ack) ->
      ack *<= ()
       
    | Flush (ackCh, nack) ->
      Ch.give ackCh () <|> nack :> Job<_>
      

  let extractMessage request = 
    match request with
    | Log (msg, ack) -> 
      Serialisation.serialiseMessage msg
    | _ ->
      ""

  let loop (conf : InfluxDbConf) (ri : RuntimeInfo)
           (requests : RingBuffer<_>)
           (shutdown : Ch<_>) =
    let endpoint =
      let ub = UriBuilder(conf.endpoint)
      ub.Path <- "/write"
      ub.Query <- "db=" + conf.db
      ub.Uri

    let tryAddAuth conf =
      conf.username
      |> Option.bind (fun u -> conf.password |> Option.map (fun p -> u, p))
      |> Option.fold (fun _ (u, p) -> Request.basicAuthentication u p) id

    let rec loop () : Job<unit> =
      Alt.choose [
        shutdown ^=> fun ack ->
          ack *<= () :> Job<_>

        // 'When there is a request' call this function
        RingBuffer.takeBatch (uint32 conf.batchSize) requests ^=> fun reqs ->

          let messageTexts = Seq.map extractMessage reqs
          let body = messageTexts |> String.concat "\n"

          job {
            let req =
              Request.create Post endpoint
              |> Request.keepAlive true
              |> Request.body (BodyString body)
              |> tryAddAuth conf
            use! resp = getResponse req
            let! body = Response.readBodyAsString resp
            if resp.statusCode > 299 then
              do! Logger.log ri.logger (
                    Message.event Error "problem receiving response"
                    |> Message.setField "statusCode" resp.statusCode
                    |> Message.setField "body" body)
              failwithf "got response code %i" resp.statusCode
            else
              do! Seq.iterJobIgnore reqestAckJobCreator reqs
              return! loop ()
          }
      ] :> Job<_>
    loop ()

/// Create a new Noop target
let create conf = TargetUtils.stdNamedTarget (Impl.loop conf)

/// Use with LogaryFactory.New( s => s.Target<InfluxDb.Builder>() )
type Builder(conf, callParent : FactoryApi.ParentCallback<Builder>) =
  // TODO: expose for C# peeps
  member x.WriteEndpoint(writeEndpoint : Uri) =
    ! (callParent <| Builder({ conf with endpoint = writeEndpoint }, callParent))

  new(callParent : FactoryApi.ParentCallback<_>) =
    Builder(empty, callParent)

  interface Logary.Target.FactoryApi.SpecificTargetConf with
    member x.Build name = create conf name
