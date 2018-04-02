module Logary.Targets.InfluxDb

open System
open System.Text
open System.Collections.Generic
open Hopac
open Hopac.Infixes
open Hopac.Extensions
open HttpFs.Client
open HttpFs.Composition
open Logary
open Logary.Message
open Logary.Internals
open Logary.Internals.TypeShape.Core
open Logary.Configuration

/// An implementation for the InfluxDb-specific string format.
module Serialise =

  /// https://docs.influxdata.com/influxdb/v1.5/write_protocols/line_protocol_reference/
  [<Struct>]
  type Escaped =
    private Escaped of value:string
  with
    member x.isEmpty =
      let (Escaped s) = x in String.IsNullOrWhiteSpace s
    static member internal create value =
      if isNull value then nullArg "value"
      Escaped value

    static member fieldValue (nonEscapedString: string) =
      // only replace " because we put quotes around the string
      let inner = nonEscapedString.Replace("\"", "\\\"") // " -> \"
      Escaped (sprintf "\"%s\"" inner)

    static member fieldValue (b: bool) =
      Escaped (if b then "T" else "F")

    static member fieldValue (f: float) =
      Escaped (f.ToString Culture.invariant)

    static member fieldValue (i: int64) =
      Escaped (i.ToString(Culture.invariant) + "i")

    static member fieldValue (v: Value) =
      match v with
      | Float v ->
        Escaped (v.ToString(Culture.invariant))
      | Int64 v ->
        Escaped (v.ToString(Culture.invariant) + "i")
      | BigInt v ->
        if v > bigint Int64.MaxValue then
          Escaped (Int64.MaxValue.ToString(Culture.invariant) + "i")
        elif v < bigint Int64.MinValue then
          Escaped (Int64.MinValue.ToString(Culture.invariant) + "i")
        else
          Escaped (v.ToString(Culture.invariant) + "i")
      | Fraction _ as v ->
        Escaped (v.toFloat().ToString(Culture.invariant))

    static member private escape (nonEscapedString: string) =
      nonEscapedString
        .Replace(",", @"\,")
        .Replace(" ", "\ ")
        .Replace("=", "\=")
      |> Escaped

    static member fieldName (nonEscapedString: string) =
      Escaped.escape nonEscapedString

    static member tagValue (nonEscapedString: string) =
      Escaped.escape nonEscapedString

    static member tagName (nonEscapedString: string) =
      Escaped.escape nonEscapedString

    override x.ToString () =
      let (Escaped s) = x in s

  let timestamp (ts: EpochNanoSeconds): Escaped =
    ts.ToString Culture.invariant |> Escaped.create

  let private anyValue (x: obj): Escaped option =
    match x with
    | :? string as s -> Some (Escaped.fieldValue s)
    | :? uint16 as i -> Some (Escaped.fieldValue (int64 i))
    | :? uint32 as i -> Some (Escaped.fieldValue (int64 i))
    | :? uint64 as i -> Some (Escaped.fieldValue (BigInt (bigint i)))
    | :? int16 as i -> Some (Escaped.fieldValue (int64 i))
    | :? int32 as i -> Some (Escaped.fieldValue (int64 i))
    | :? int64 as i -> Some (Escaped.fieldValue i)
    | :? float as f -> Some (Escaped.fieldValue f)
    | :? decimal as d -> Some (Escaped.fieldValue (float d))
    | :? bool as b when b -> Some (Escaped.fieldValue b)
    | :? bool as b -> Some (Escaped.fieldValue b)
    | _ -> None

  let tag (tvalue: obj): Escaped option =
    anyValue tvalue

  let tags (xs: #seq<Escaped * Escaped>): Escaped =
    if Seq.isEmpty xs then Escaped.create ""
    else
      xs
      |> Seq.sortBy fst
      |> Seq.map (fun (x, y) -> sprintf "%O=%O" x y)
      |> String.concat ","
      |> sprintf ",%s"
      |> Escaped.create

  let field (fvalue: obj): Escaped option =
    anyValue fvalue

  let fields (xs: #seq<Escaped * Escaped>): Escaped =
    if Seq.isEmpty xs then Escaped.create "value=0"
    else
      xs
      |> Seq.sortBy fst
      |> Seq.map (fun (x, y) -> sprintf "%O=%O" x y)
      |> String.concat ","
      |> Escaped.create

  open Logary.Message.Patterns

  let nameGauges (message: Message) (gauges: Dictionary<string, Escaped * string option>) =
    let inline formatValueLiteral (v, x) =
      match v, x with
      | v, None ->
        [| Escaped.create "value", v |]
      | v, Some (formatted: string) ->
        [| Escaped.create "value", v; Escaped.create "value_f", Escaped.fieldValue formatted |]

    let allGaugesBut ignoredKey =
      gauges |> Seq.collect (function
        | KeyValue (k, _) when Some k = ignoredKey ->
          [||]
        | KeyValue (k, (v, None)) ->
          [| Escaped.fieldName k, v |]
        | KeyValue (k, (v, Some formatted)) ->
          [| Escaped.fieldName k, v; Escaped.create (sprintf "%O_f" k), Escaped.fieldValue formatted |])

    let singleGaugeAsValue =
      gauges |> Seq.collect (fun (KeyValue (_, x)) -> formatValueLiteral x)

    match message.name, message.value, gauges.Count with
    // Empty event message
    | PointName.Empty, template, 0 when not (String.IsNullOrWhiteSpace template) ->
      Escaped.tagName template,
      Seq.singleton (Escaped.create "value", Escaped.create "1i")

    // Fully empty message
    | PointName.Empty, _, 0 ->
      Escaped.tagName "MISSING",
      Seq.singleton (Escaped.create "value", Escaped.create "0")

    // Event message with sensor/logger/measurement name, otherwise empty
    | sensor, _, 0 ->
      Escaped.tagName (PointName.format sensor),
      Seq.singleton (Escaped.create "value", Escaped.create "1i")

    // Non-empty event/template/message
    | PointName.Empty, template, 1 when not (String.IsNullOrWhiteSpace template) ->
      let measurement =
        PointName.parse template
        |> PointName.setEnding (Seq.head gauges.Keys)
        |> PointName.format
      Escaped.tagName measurement,
      singleGaugeAsValue

    // Empty sensor/logger/measurement name and event/template/message, use gauge name
    | PointName.Empty, _, 1 ->
      Escaped.tagName (Seq.head gauges.Keys),
      singleGaugeAsValue

    // Non-empty sensor/logger/measurement, single gauge, append to measurement name
    | sensor, _, 1 ->
      let measurement =
        sensor
        |> PointName.setEnding (Seq.head gauges.Keys)
        |> PointName.format
      Escaped.tagName measurement,
      singleGaugeAsValue

    // Multi-gauge message with event/template/message.
    | PointName.Empty, template, n when not (String.IsNullOrWhiteSpace template) ->
      Escaped.tagName template,
      allGaugesBut None

    // Multi-gauge message without event/template/message; pick first gauge as measurement (??)
    | PointName.Empty, _, _ ->
      let first = Seq.head gauges.Keys
      Escaped.tagName first,
      Seq.concat [
        formatValueLiteral gauges.[first] :> seq<_>
        allGaugesBut (Some first)
      ]

    // We have a measurement name and multiple gauges
    | sensor, _, _ ->
      Escaped.tagName (PointName.format sensor),
      allGaugesBut None

  let message (message: Message): string =
    let ts = ResizeArray<_>() // tags
    let fs = ResizeArray<_>() // fields
    let gauges = Dictionary<string, _ * _ option>()

    for kv in message.context do
      match kv with
      | Intern -> ()

      | Tags tags ->
        tags
        |> Seq.map (fun t -> Escaped.tagName t, Escaped.tagValue "true")
        |> ts.AddRange

      | Field (fname, fvalue) ->
        field fvalue |> Option.iter (fun x -> fs.Add (Escaped.fieldName fname, x))

      | Context (key, cvalue) ->
        tag cvalue |> Option.iter (fun x -> ts.Add (Escaped.tagName key, x))

      | Gauge (gname, g) ->
        gauges.[gname] <-
          (Escaped.fieldValue g.value,
           if g.unit = Scalar then None
           else Some (Units.formatWithUnit Units.Suffix g.unit g.value))

    let measurement, gaugeFields = nameGauges message gauges
    fs.AddRange gaugeFields
    ts.Add (Escaped.tagName "level", Escaped.tagValue (message.level.ToString()))

    let sb = new StringBuilder()
    let app (chars: string) = sb.Append chars |> ignore
    app (sprintf "%O" measurement)
    app (tags ts |> sprintf "%O")
    app " "
    app (fields fs |> sprintf "%O")
    app " "
    app (timestamp message.timestamp |> sprintf "%O")
    sb.ToString()

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
  { endpoint: Uri
    /// REQUIRED - sets the target database for the write
    db: string
    /// if authentication is enabled, you must authenticate as a user with write permissions to the target database
    username: string option
    /// if authentication is enabled, you must authenticate as a user with write permissions to the target database
    password: string option
    /// set the number of nodes that must confirm the write. If the requirement is not met the return value will be partial write if some points in the batch fail, or write failure if all points in the batch fail.
    consistency: Consistency
    /// sets the target retention policy for the write. If not present the default retention policy is used
    retention: string option
    /// Sets how many measurements should be batched together if new measurements are produced faster than we can write them one by one. Default is 100.
    batchSize: uint16 }

  static member create(ep, db, ?user, ?password, ?consistency, ?retention, ?batchSize) =
    { endpoint = ep
      db = db
      username = defaultArg user None
      password = defaultArg password None
      consistency = defaultArg consistency Quorum
      retention = defaultArg retention None
      batchSize = defaultArg batchSize 100us }

let empty =
  { endpoint    = Uri "http://127.0.0.1:8086/write"
    db          = "logary"
    username    = None
    password    = None
    consistency = Quorum
    retention   = None
    batchSize   = 100us }

module internal Impl =
  open System.Net.Http
  open System.Text
  open Option.Operators

  let endpoint (conf: InfluxDbConf) =
    let ub = UriBuilder(conf.endpoint)
    ub.Path <- "/write"
    ub.Query <- "db=" + conf.db
    ub.Uri

  let tuple a b = a, b

  /// Ensures the request is authenticated, should the configuration contain credentials.
  let auth conf: JobFilter<Request, Response> =
    match tuple <!> conf.username <*> conf.password with
    | Some (username, password) ->
      fun next req ->
        req
        |> Request.basicAuthentication username password
        |> next
    | None ->
      fun next req ->
        next req

  /// Guards so that all sent messages are successfully written.
  let guardRespCode (runtime: RuntimeInfo) (body, statusCode) =
    if statusCode >= 200 && statusCode <= 299 then
      Job.result ()
    else
      runtime.logger.logWithAck Error (
        eventX "InfluxDb target received response {statusCode} with {body}."
        >> setField "statusCode" statusCode
        >> setField "body" body)
      |> Job.bind id
      |> Job.bind (fun () -> Job.raises (Exception body))

  let bodyAndCode (resp: Response) =
    resp
    |> Job.useIn Response.readBodyAsString
    |> Job.map (fun body -> body, resp.statusCode)

  // Move to Composition
  let codec enc dec =
    fun next inp ->
      next (enc inp) |> Alt.afterJob dec

  // Move to Composition
  let sinkJob (sink: _ -> #Job<unit>) =
    fun next inp ->
      next inp |> Alt.afterJob sink

  // Move to Composition
  module private JobFunc =
    let bind (f: 'b -> #Job<'c>) (func: JobFunc<'a, 'b>): JobFunc<'a, 'c> =
      func >> Alt.afterJob f

  type State =
    { client: HttpClient
      send: string -> Alt<unit> }

    interface IDisposable with
      member x.Dispose() =
        x.client.Dispose()

    static member create (conf: InfluxDbConf) (runtime: RuntimeInfo) =
      let client, endpoint = new HttpClient(), endpoint conf

      let create body =
        Request.createWithClient client Post endpoint
        |> Request.bodyString body

      let filters: JobFilter<Request, Response, string, unit> =
        auth conf
        >> codec create bodyAndCode
        >> sinkJob (guardRespCode runtime)

      { client = client; send = filters getResponse }

  let loop (conf: InfluxDbConf) (runtime: RuntimeInfo, api: TargetAPI) =
    runtime.logger.info (
      eventX "Started InfluxDb target with endpoint {endpoint}."
      >> setField "endpoint" (endpoint conf))

    let rec loop (state: State): Job<unit> =
      Alt.choose [
        api.shutdownCh ^=> fun ack ->
          runtime.logger.verbose (eventX "Shutting down InfluxDb target.")
          ack *<= () :> Job<_>

        RingBuffer.takeBatch conf.batchSize api.requests ^=> fun messages ->
          let entries, acks, flushes =
            messages |> Array.fold (fun (entries, acks, flushes) -> function
              | Log (message, ack) ->
                Serialise.message message :: entries,
                ack *<= () :: acks,
                flushes
              | Flush (ackCh, nack) ->
                entries,
                acks,
                ackCh *<= () :: flushes)
              ([], [], [])

          job {
            do runtime.logger.verbose (eventX "Writing {count} messages" >> setField "count" (entries.Length))
            do! entries |> String.concat "\n" |> state.send
            do runtime.logger.verbose (eventX "Acking messages")
            do! Job.conIgnore acks
            do! Job.conIgnore flushes
            return! loop state
          }
      ] :> Job<_>

    job {
      use state = State.create conf runtime
      return! loop state
    }

/// Create a new InfluxDb target.
[<CompiledName "Create">]
let create conf name =
  TargetConf.createSimple (Impl.loop conf) name

/// Use with LogaryFactory.New( s => s.Target<InfluxDb.Builder>() )
type Builder(conf, callParent: Target.ParentCallback<Builder>) =
  let update (conf': InfluxDbConf): Builder =
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
  member x.WriteEndpoint(writeEndpoint: Uri) =
    update { conf with endpoint = writeEndpoint }

  /// You've finished configuring the InfluxDb target.
  member x.Done() =
    ! (callParent x)

  new(callParent: Target.ParentCallback<_>) =
    Builder(empty, callParent)

  interface Target.SpecificTargetConf with
    member x.Build name = create conf name
