module Logary.Targets.InfluxDb

open System
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
      nonEscapedString
        .Replace(",", @"\,")
        .Replace(" ", "\ ")
        .Replace("=", "\=")
      |> Escaped
    static member fieldName (nonEscapedString: string) =
      Escaped.fieldValue nonEscapedString
    static member tagValue (nonEscapedString: string) =
      Escaped.fieldValue nonEscapedString
    static member tagName (nonEscapedString: string) =
      Escaped.fieldValue nonEscapedString
    override x.ToString () =
      let (Escaped s) = x in s

  let timestamp (ts: EpochNanoSeconds): Escaped =
    Escaped.create (ts.ToString(Culture.invariant))

  let units: Units -> Escaped option = function
    | Scalar ->
      None
    | other ->
      Some (Escaped.fieldValue other.symbol)

  let value: Value -> Escaped = function
    | Float v ->
      Escaped.create (v.ToString(Culture.invariant))
    | Int64 v ->
      Escaped.create (v.ToString(Culture.invariant) + "i")
    | BigInt v ->
      if v > bigint Int64.MaxValue then
        Escaped.create (Int64.MaxValue.ToString(Culture.invariant) + "i")
      elif v < bigint Int64.MinValue then
        Escaped.create (Int64.MinValue.ToString(Culture.invariant) + "i")
      else
        Escaped.create (v.ToString(Culture.invariant) + "i")
    | Fraction _ as v ->
      Escaped.create (v.toFloat().ToString(Culture.invariant))

  let gaugeName (gname: string) (g: Gauge) =
    match g.unit with
    | Scalar ->
      gname
    | _ ->
      sprintf "%s[%s]" gname g.unit.symbol

  let str (s: string) = // ??
    s.Replace("\"", "\\\"")

  let strQ (s: string) = // ??
    "\"" + str s + "\""

  let bool (b: bool): Escaped =
    Escaped.create (if b then "true" else "false")

  let field (f: obj): Escaped option =
    let t = f.GetType()
    if t.IsPrimitive then
      Some (Escaped.fieldValue (t.ToString()))
    else
      None

  let tags (xs: #seq<Escaped * Escaped>) =
    if Seq.isEmpty xs then "" else
    xs
      |> Seq.map (fun (x, y) -> sprintf "%O=%O" x y)
      |> String.concat ","
      |> sprintf ",%s"

  let fields (xs: #seq<Escaped * Escaped>) =
    if Seq.isEmpty xs then "value=0" else
    xs
      |> Seq.map (fun (x, y) -> sprintf "%O=%O" x y)
      |> String.concat ","

  open Logary.Message.Patterns

  let message (message: Message): string =
    let ts = ResizeArray<_>()
    let fs = ResizeArray<_>()
    let measurement = ref (Escaped.create "")

    if message.name <> PointName.empty then
      measurement := Escaped.tagName (PointName.format message.name)

    elif message.value <> String.Empty then
      measurement := Escaped.tagName message.value

    ts.Add (Escaped.tagName "level", Escaped.tagValue (message.level.ToString()))

    for kv in message.context do
      match kv with
      | Unmarked | Intern -> ()
      | Tags tags ->
        ts.AddRange (tags |> Seq.map (fun t -> Escaped.tagName t, Escaped.tagValue "true"))

      | Field (fname, f) ->
        field f |> Option.iter (fun f -> fs.Add (Escaped.fieldName fname, f))

      | Gauge (gname, g) ->
        if (!measurement).isEmpty then
          measurement := Escaped.tagName gname
          fs.Add (Escaped.create "value", value g.value)
          units g.unit |> Option.iter (fun u -> fs.Add (Escaped.create "unit", u))
        else
          let nameUnit = gaugeName gname g
          fs.Add (Escaped.fieldName nameUnit, value g.value)

        let formatted = Units.formatWithUnit Units.Suffix g.unit g.value
        fs.Add (Escaped.fieldName "formatted", Escaped.fieldValue formatted)

    sprintf "%O%s %s %O" !measurement (tags ts) (fields fs) (timestamp message.timestamp)

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

        RingBuffer.takeBatch (conf.batchSize) api.requests ^=> fun messages ->
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
