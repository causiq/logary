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

    static member fieldKey (nonEscapedString: string) =
      Escaped.escape nonEscapedString

    static member tagValue (nonEscapedString: string) =
      Escaped.escape nonEscapedString
    static member tagValue (value: float) =
      Escaped (value.ToString Culture.invariant)
    static member tagValue (value: int64) =
      Escaped (value.ToString Culture.invariant)
    static member tagValue (value: bigint) =
      Escaped (value.ToString Culture.invariant)
    static member tagValue (value: bool) =
      Escaped (if value then "T" else "F")

    static member tagKey (nonEscapedString: string) =
      Escaped.escape nonEscapedString

    override x.ToString () =
      let (Escaped s) = x in s

  let timestamp (ts: EpochNanoSeconds): Escaped =
    ts.ToString Culture.invariant |> Escaped.create

  let tagValue (tvalue: obj): Escaped option =
    match tvalue with
    | :? string as s -> Some (Escaped.tagValue s)
    | :? uint16 as i -> Some (Escaped.tagValue (int64 i))
    | :? uint32 as i -> Some (Escaped.tagValue (int64 i))
    | :? uint64 as i -> Some (Escaped.tagValue (i.ToString Culture.invariant))
    | :? int16 as i -> Some (Escaped.tagValue (int64 i))
    | :? int32 as i -> Some (Escaped.tagValue (int64 i))
    | :? int64 as i -> Some (Escaped.tagValue i)
    | :? float as f -> Some (Escaped.tagValue f)
    | :? decimal as d -> Some (Escaped.tagValue (float d))
    | :? bool as b when b -> Some (Escaped.tagValue b)
    | :? bool as b -> Some (Escaped.tagValue b)
    | _ -> None

  let tags (xs: Dictionary<Escaped, Escaped>): Escaped =
    if Seq.isEmpty xs then Escaped.create ""
    else
      xs
      |> Seq.sortBy (fun kv -> kv.Key)
      |> Seq.map (fun kv -> sprintf "%O=%O" kv.Key kv.Value)
      |> String.concat ","
      |> sprintf ",%s"
      |> Escaped.create

  let fieldValue (fvalue: obj): Escaped option =
    match fvalue with
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


  let fields (xs: #seq<Escaped * Escaped>): Escaped =
    if Seq.isEmpty xs then Escaped.create "value=0"
    else
      xs
      |> Seq.sortBy fst
      |> Seq.map (fun (x, y) -> sprintf "%O=%O" x y)
      |> String.concat ","
      |> Escaped.create

  open Logary.Message.Patterns

  let nameGauges (message: Message) (gauges: Dictionary<string, Escaped * string option * string option>) =
    let inline formatLiteral rawName (v, format: string option, units: string option): (Escaped * Escaped)[] =
      [|
          yield Escaped.fieldKey rawName, v
          if Option.isSome format then
            yield Escaped.fieldKey (sprintf "%s_f" rawName), Escaped.fieldValue (Option.get format)
          if Option.isSome units then
            yield Escaped.fieldKey (sprintf "%s_unit" rawName), Escaped.fieldValue (Option.get units)
      |]

    let allGaugesBut ignoredKey =
      gauges |> Seq.collect (function
        | KeyValue (k, _) when Some k = ignoredKey -> [||]
        | KeyValue (k, x) -> formatLiteral k x)

    let singleGaugeAsValue =
      gauges |> Seq.collect (fun (KeyValue (_, x)) -> formatLiteral "value" x)

    match message.name, message.value, gauges.Count with
    // Fully empty message
    | PointName.Empty, template, 0 when String.IsNullOrWhiteSpace template ->
      Escaped.tagKey "MISSING",
      Seq.singleton (Escaped.create "value", Escaped.create "0")

    // Empty event message, the message becomes the measurement
    | PointName.Empty, template, 0 ->
      Escaped.tagKey template,
      Seq.singleton (Escaped.create "value", Escaped.create "1i")

    // Event message with sensor/logger/measurement name, otherwise empty,
    // the message becomes an event (this is the regular log message case)
    | sensor, template, 0 ->
      Escaped.tagKey (PointName.format sensor),
      [|
          Escaped.create "value", Escaped.create "1i"
          Escaped.create "event", Escaped.fieldValue template
      |]
      :> seq<_>

    // Non-empty event/template/message
    | PointName.Empty, template, 1 when not (String.IsNullOrWhiteSpace template) ->
      let measurement =
        PointName.parse template
        |> PointName.setEnding (Seq.head gauges.Keys)
        |> PointName.format
      Escaped.tagKey measurement,
      singleGaugeAsValue

    // Empty sensor/logger/measurement name and event/template/message, use gauge name
    | PointName.Empty, _, 1 ->
      Escaped.tagKey (Seq.head gauges.Keys),
      singleGaugeAsValue

    // Non-empty sensor/logger/measurement, single gauge, append to measurement name
    | sensor, _, 1 ->
      let measurement =
        sensor
        |> PointName.setEnding (Seq.head gauges.Keys)
        |> PointName.format
      Escaped.tagKey measurement,
      singleGaugeAsValue

    // Multi-gauge message with event/template/message.
    | PointName.Empty, template, n when not (String.IsNullOrWhiteSpace template) ->
      Escaped.tagKey template,
      allGaugesBut None

    // Multi-gauge message without event/template/message; pick first gauge as measurement (??)
    | PointName.Empty, _, _ ->
      let first = Seq.head gauges.Keys
      Escaped.tagKey first,
      Seq.concat [
        formatLiteral "value" gauges.[first] :> seq<_>
        allGaugesBut (Some first)
      ]

    // We have a measurement name and multiple gauges
    | sensor, _, _ ->
      Escaped.tagKey (PointName.format sensor),
      allGaugesBut None

  let message (message: Message): string =
    let ts = Dictionary<Escaped, Escaped>() // tags
    let fs = ResizeArray<_>() // fields
    let gauges = Dictionary<string, Escaped * string option * string option>()

    for kv in message.context do
      match kv with
      | Intern -> ()

      | Tags tags ->
        ts.[Escaped.tagKey "tags"] <-
          tags
          |> Seq.filter (String.IsNullOrWhiteSpace >> not)
          |> Seq.sortBy id
          |> String.concat ","
          |> Escaped.tagValue

      | Field (fname, fvalue) ->
        fieldValue fvalue |> Option.iter (fun x -> fs.Add (Escaped.fieldKey fname, x))

      | Context (key, cvalue) ->
        tagValue cvalue |> Option.iter (fun x -> ts.Add (Escaped.tagKey key, x))

      | Gauge (gname, g) ->
        let value = Escaped.fieldValue g.value
        let units =
          if g.unit = Scalar then
            None
          else
            let scaled, units = Units.scale g.unit (g.value.toFloat())
            // 2 decimal places, 3-5 sig-figs, see https://msdn.microsoft.com/visualfsharpdocs/conceptual/core.printf-module-%5bfsharp%5d
            Some (sprintf "%0.2f %s" scaled units)
        gauges.[gname] <- (value, units, g.unit.name)

    ts.Add (Escaped.tagKey "level", Escaped.tagValue (message.level.ToString()))

    let measurement, gaugeFields = nameGauges message gauges
    fs.AddRange gaugeFields

    let sb = new StringBuilder()
    let app (chars: string) = sb.Append chars |> ignore
    app (measurement.ToString())
    app ((tags ts).ToString())
    app " "
    app ((fields fs).ToString())
    app " "
    app ((timestamp message.timestamp).ToString())
    sb.ToString()

type Consistency =
  /// The data must be written to disk by at least 1 valid node
  | One
  /// The data must be written to disk by (N/2 + 1) valid nodes (N is the replication factor for the target retention policy)
  | Quorum
  /// The data must be written to disk by all valid nodes
  | All
  /// a write is confirmed if hinted-handoff is successful, even if all target
  /// nodes report a write failure In this context, “valid node” means a node
  /// that hosts a copy of the shard containing the series being written to. In
  /// a clustered system, the replication factor should equal the number of
  /// valid nodes.
  | Any

type InfluxDbConf =
    /// The write endpoint to send the values to
  { endpoint: Uri
    /// REQUIRED - sets the target database for the write
    db: string
    /// If authentication is enabled, you must authenticate as a user with write permissions to the target database
    username: string option
    /// If authentication is enabled, you must authenticate as a user with write permissions to the target database
    password: string option
    /// Set the number of nodes that must confirm the write. If the requirement is not met the return value will be partial write if some points in the batch fail, or write failure if all points in the batch fail.
    consistency: Consistency
    /// Sets the target retention policy for the write. If not present the default retention policy is used
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

  let loop (conf: InfluxDbConf) (api: TargetAPI) =
    let logger = api.runtime.logger
    logger.info (
      eventX "Started InfluxDb target with endpoint {endpoint}."
      >> setField "endpoint" (endpoint conf))

    let rec loop (state: State): Job<unit> =
      Alt.choose [
        api.shutdownCh ^=> fun ack ->
          logger.verbose (eventX "Shutting down InfluxDb target.")
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
            |> fun (es, aas, fls) -> List.rev es, List.rev aas, List.rev fls

          job {
            do logger.verbose (eventX "Writing {count} messages" >> setField "count" (entries.Length))
            do! entries |> String.concat "\n" |> state.send
            do logger.verbose (eventX "Acking messages")
            do! Job.conIgnore acks
            do! Job.conIgnore flushes
            return! loop state
          }
      ] :> Job<_>

    job {
      use state = State.create conf api.runtime
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