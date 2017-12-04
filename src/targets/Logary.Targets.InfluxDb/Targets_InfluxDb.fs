module Logary.Targets.InfluxDb

open Hopac
open Hopac.Infixes
open Hopac.Extensions
open HttpFs.Client
open Logary
open Logary.Message
open Logary.Internals
open Logary.Configuration
open System

/// An implementation for the InfluxDb-specific string format.
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

  let printFields = function
    | [] ->
      "value=0"
    | xs ->
      // TO CONSIDER: assumes values are escaped
      xs |> List.map (fun (x, y) -> sprintf "%s=%s" (escapeString x) y)
         |> String.concat ","

  let serialiseMessage (message : Message) : string =
    failwith "TODO: needs to be discussed"

    // let mapExtract fExtractValue =
    //   Map.map (fun k -> fExtractValue >> Option.orDefault "")
    //   >> Seq.filter (fun (KeyValue (k, v)) -> v <> String.Empty)
    //   >> Seq.map (fun (KeyValue (k, v)) -> k, v)
    //   >> Seq.sortBy fst
    //   >> List.ofSeq

    // let rec getValues (valueKey : string)
    //                   isTag
    //                   (kvs : (string * string) list)
    //                   (values : (string * string) list) =
    //   function
    //   | Float v ->
    //     kvs, (valueKey, v.ToString(Culture.invariant)) :: values

    //   | Int64 v ->
    //     kvs, (valueKey, v.ToString(Culture.invariant) + "i") :: values

    //   | String s ->
    //     let fser = if isTag then serialiseStringTag else serialiseStringValue
    //     kvs, (valueKey, fser s) :: values

    //   | Bool true ->
    //     kvs, (valueKey, "true") :: values

    //   | Bool false ->
    //     kvs, (valueKey, "false") :: values

    //   | BigInt v ->
    //     if v > bigint Int64.MaxValue then
    //       let str = Int64.MaxValue.ToString(Culture.invariant) + "i"
    //       kvs, (valueKey, str) :: values
    //     elif v < bigint Int64.MinValue then
    //       let str = Int64.MinValue.ToString(Culture.invariant) + "i"
    //       kvs, (valueKey, str) :: values
    //     else
    //       let str = v.ToString(Culture.invariant) + "i"
    //       kvs, (valueKey, str) :: values

    //   | Binary (bs, ct) ->
    //     ("value_ct", ct) :: kvs,
    //     (valueKey, Convert.ToBase64String(bs)) :: values

    //   | Fraction (n, d) ->
    //     kvs,
    //     (valueKey, (n / d).ToString(Culture.invariant)) :: values

    //   | Object _ as o ->
    //     complexValue valueKey kvs values o

    //   | Array _ as a ->
    //     complexValue valueKey kvs values a

    // and extractSimple valueKey isTag v =
    //   match getValues valueKey isTag [] [] v with
    //   | _, (_, str) :: _ -> Some str
    //   | _ -> None

    // and complexValue valueKey kvs values = function
    //   | Object mapKvs ->
    //     let newValues = mapKvs |> mapExtract (extractSimple valueKey false)
    //     kvs, newValues @ values

    //   | Array arr ->
    //     let rec array i acc = function
    //       | [] ->
    //         acc

    //       | h :: tail ->
    //         match extractSimple valueKey false h with
    //         | Some str ->
    //           array (i + 1) ((sprintf "arr_%i" i, str) :: acc) tail
    //         | None ->
    //           array i acc tail

    //     kvs, array 0 [] arr

    //   | x ->
    //     failwithf "'%A' is not a complex value" x

    // let simpleValue = getValues "value"

    // let removeSuppressTag suppress =
    //   if suppress then
    //     Map.map (fun k v ->
    //       match k with
    //       | KnownLiterals.TagsContextName ->
    //         match v with
    //         | Array tags ->
    //           Value.Array (tags |> List.filter ((<>) (Value.String KnownLiterals.SuppressPointValue)))
    //         | _ ->
    //           v
    //       | _ -> v)
    //   else id

    // let contextValues suppress context : PointValue -> _ * _ =
    //   let context' =
    //     context
    //     |> removeSuppressTag suppress
    //     |> mapExtract (extractSimple "value" true)

    //   function
    //   | Gauge (value, Scalar)
    //   | Derived (value, Scalar) ->
    //     let contextTags, contextFields = simpleValue false context' [] value
    //     if suppress then contextTags, [] else contextTags, contextFields

    //   | Gauge (value, units)
    //   | Derived (value, units) ->
    //     simpleValue false context' [] value |> fun (tags, fields) ->
    //     let contextTags, contextFields =
    //       ("unit", serialiseStringTag (Units.symbol units)) :: tags,
    //       fields
    //     if suppress then contextTags, [] else contextTags, contextFields

    //   | Event templ ->
    //     let kvs, values = simpleValue false context' [] (Value.Int64 1L)
    //     kvs,
    //     // events' templates vary a lot, so don't index them
    //     ("event", serialiseStringValue templ) :: values

    // let fieldValues (fields : Map<PointName, Field>)  =
    //   Map.toSeq fields
    //   |> Seq.map (fun (key, (Field (value, _))) -> PointName.format key, value)
    //   |> Map.ofSeq
    //   |> Value.Object
    //   |> getValues "value" false [] []

    // let measurementName (m : Message) =
    //   match m.value with
    //   | Event _ ->
    //     // events should result in measurements like "event_info" or "event_fatal"
    //     // because that's how they're queried
    //     escapeString (sprintf "event_%O" m.level)
    //   | _ ->
    //     // whilst measurements should result in measurement names equivalent to thei
    //     // point names
    //     serialisePointName m.name

    // let extraFields pointName = function
    //   // pass the point name of the event as an extra field
    //   | Event _ ->
    //     [ "pointName", serialiseStringValue (PointName.format pointName) ]
    //   | _ ->
    //     []

    // let suppress = message |> Message.hasTag KnownLiterals.SuppressPointValue
    // let fieldTags, fieldFields = fieldValues message.fields
    // let contextTags, contextFields = contextValues suppress message.context message.value
    // let extraFields = extraFields message.name message.value

    // sprintf "%O%s %s %i"
    //         (measurementName message)
    //         (printTags (List.concat [fieldTags; contextTags]))
    //         (printFields (List.concat [extraFields; fieldFields; contextFields]))
    //         message.timestamp

type Consistency =
  /// the data must be written to disk by at least 1 valid node
  | One
  /// the data must be written to disk by (N/2 + 1) valid nodes (N is the replication factor for the target retention policy)
  | Quorum
  ///  the data must be written to disk by all valid nodes
  | All
  /// a write is confirmed if hinted-handoff is successful, even if all target
  /// nodes report a write failure In this context, “valid node” means a node
  /// that hosts a copy of the shard containing the series being written to. In
  /// a clustered system, the replication factor should equal the number of
  /// valid nodes.
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

module internal Impl =
  open System.Text

  let reqestAckJobCreator request =
    match request with
    | Log (msg, ack) ->
      ack *<= ()
    | Flush (ackCh, nack) ->
      IVar.fill ackCh ()

  let extractMessage request = 
    match request with
    | Log (msg, ack) -> 
      Serialisation.serialiseMessage msg
    | _ ->
      ""

  let loop (conf : InfluxDbConf) (ri : RuntimeInfo, api : TargetAPI) =
    let endpoint =
      let ub = UriBuilder(conf.endpoint)
      ub.Path <- "/write"
      ub.Query <- "db=" + conf.db
      ub.Uri

    let tryAddAuth conf =
      conf.username
      |> Option.bind (fun u -> conf.password |> Option.map (fun p -> u, p))
      |> Option.fold (fun _ (u, p) -> Request.basicAuthentication u p) id

    let g (req : Request) = job {
      use! resp = getResponse req
      let! body = Response.readBodyAsString resp
      return body, resp.statusCode
    }

    let rec loop () : Job<unit> =
      Alt.choose [
        api.shutdownCh ^=> fun ack ->
          ack *<= () :> Job<_>

        RingBuffer.takeBatch (conf.batchSize) api.requests ^=> fun reqs ->
          let body =
            reqs
            |> Seq.map extractMessage
            |> String.concat "\n"
          let req =
            Request.create Post endpoint
            |> Request.keepAlive true
            |> Request.bodyString body
            |> tryAddAuth conf

          job {
            do! ri.logger.verboseWithBP (eventX "Sending {body}" >> Message.setField "body" body)
            let! body, statusCode = g req
            if statusCode > 299 then
              let! errorFlush =
                ri.logger.logWithAck Error (
                  eventX  "Bad response {statusCode} with {body}"
                  >> setField "statusCode" statusCode
                  >> setField "body" body)
              do! errorFlush
              // will cause the target to restart through the supervisor
              failwithf "Bad response statusCode=%i" statusCode
            else
              do! Seq.iterJobIgnore reqestAckJobCreator reqs
              return! loop ()
          }
      ] :> Job<_>
    loop ()

/// Create a new InfluxDb target.
let create conf =
  TargetConf.createSimple (Impl.loop conf)

/// Use with LogaryFactory.New( s => s.Target<InfluxDb.Builder>() )
type Builder(conf, callParent : Target.ParentCallback<Builder>) =
  let update (conf' : InfluxDbConf) : Builder =
    Builder(conf', callParent)

  /// if authentication is enabled, you must authenticate as a user with write permissions to the target database
  member x.Authenticate (username, password) =
    update { conf with username = Some username
                       password = Some password }

  /// the data must be written to disk by at least 1 valid node
  member x.ConsistencyOne() =
    update { conf with consistency = One }

  /// the data must be written to disk by (N/2 + 1) valid nodes (N is the replication factor for the target retention policy)
  member x.ConsistencyQuorum() =
    update { conf with consistency = Quorum }

  ///  the data must be written to disk by all valid nodes
  member x.ConsistencyAll() =
    update { conf with consistency = All }

  /// a write is confirmed if hinted-handoff is successful, even if all target
  /// nodes report a write failure In this context, “valid node” means a node
  /// that hosts a copy of the shard containing the series being written to. In
  /// a clustered system, the replication factor should equal the number of
  /// valid nodes.
  member x.ConsistencyAny() =
    update { conf with consistency = Any }

  /// sets the target retention policy for the write. If not present the default retention policy is used
  member x.Retention policy =
    update { conf with retention = Some policy }

  /// Sets how many measurements should be batched together if new measurements are produced faster than we can write them one by one. Default is 100. 
  member x.BatchSize size =
    update { conf with batchSize = uint16 size }

  /// Sets the target database for the write - defaults to `logary`.
  member x.DB db =
    update { conf with db = db }

  /// the write endpoint to send the values to
  member x.WriteEndpoint(writeEndpoint : Uri) =
    update { conf with endpoint = writeEndpoint }

  /// You've finished configuring the InfluxDb target.
  member x.Done() =
    ! (callParent x)

  new(callParent : Target.ParentCallback<_>) =
    Builder(empty, callParent)

  interface Target.SpecificTargetConf with
    member x.Build name = create conf name
