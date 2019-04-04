/// You must acquire a commercial license from henrik@haf.se to use this code.
module Logary.Targets.Mixpanel

open System
open System.Text
open HttpFs.Client
open HttpFs.Composition
open Hopac
open Hopac.Infixes
open Logary
open Logary.Message
open Logary.Internals
open Logary.Internals.Chiron
open Logary.Configuration

/// A **very** simple extractor for the `userId` field/`user` field/context value.
let extractUserIdOrUserDotId (m: Message) =
  m
  |> tryGetField "userId"
  |> Option.orElse (m |> tryGetField "user")
  |> Option.orElse (m |> tryGetContext "userId")
  |> Option.map string

let extractIPField (m: Message) =
  m
  |> tryGetField "ip"
  |> Option.map string

/// An API access token
type Token = string

/// https://mixpanel.com/help/reference/http#tracking-via-http
/// The configuration record for Mixpanel communication.
type MixpanelConf =
  { batchSize: uint16
    /// The write endpoint to send the values to
    endpoint: string
    /// Authentication token used for accessing the API.
    token: Token
    /// Callback that tries to extract the `distinct_id` from the message.
    tryExtractDistinctId: Message -> string option
    /// Callback that tries to extract the `ip` field from the message. This is used
    /// by Mixpanel for geolocation.
    tryExtractIP: Message -> string option
  }
  /// Create a new Mixpanel configuration record for Logary to use for sending
  /// events to Mixpanel with.
  static member create (token, ?endpoint, ?batchSize, ?tryExtractDistinctId, ?tryExtractIP) =
    { batchSize = max 1us (min (defaultArg batchSize 50us) 50us)
      endpoint = defaultArg endpoint "https://api.mixpanel.com/"
      token = token
      tryExtractDistinctId = defaultArg tryExtractDistinctId extractUserIdOrUserDotId
      tryExtractIP = defaultArg tryExtractIP extractIPField }

module Track =
  let path = "/track"
module Engage =
  let path = "/engage"

let empty =
  MixpanelConf.create (
    Env.varDefault "MIXPANEL_TOKEN" (fun () -> "MIXPANEL_TOKEN=MISSING"))

module internal E =
  open Logary.MessagePatterns
  open Logary.Formatting

  module E = Json.Encode

  // `time`:
  // The time an event occurred. If present, the value should be a unix timestamp (seconds since midnight, January 1st, 1970 - UTC). If this property is not included in your request, Mixpanel will use the time the event arrives at the server.

  let properties (ilogger: Logger) (conf: MixpanelConf) (m: Message) =
    let baseObj =
      JsonObject.empty
      |> E.required E.int64 "time" m.timestampEpochS
      |> E.required E.string "token" conf.token
      |> E.required E.string "formatted" (MessageWriter.verbatim.format m)
      |> E.optional E.string "distinct_id" (conf.tryExtractDistinctId m)
      |> E.optional E.string "ip" (conf.tryExtractIP m)

    let jObj =
      (baseObj, m.context)
      ||> Seq.fold (fun o -> function
        | Intern ->
          o
        | Field (key, value) ->
          o |> JsonObject.add key (Json.encode value)
        | Gauge (key, g) ->
          o
            |> JsonObject.add key (Json.encode g)
            |> JsonObject.add (sprintf "%s_f" key) (Json.String (Gauge.format g))
        | Tags tags ->
          o |> JsonObject.add "tags" (E.stringSet tags)
        | Context (key, value) ->
          o |> JsonObject.add key (Json.encode value))

    if Option.isNone (JsonObject.tryFind "distinct_id" jObj) then
      ilogger.info (eventX "Missing `distinct_id` from message {message}" >> setField "message" m)

    Json.Object jObj

  let event (m: Message) =
    if m.value = "" then None
    else Some (Json.String m.value)

  let message (ilogger: Logger) (conf: MixpanelConf) (m: Message): _ option =
    event m |> Option.map (fun e ->
    E.propertyList [
      "properties", properties ilogger conf m
      "event", e
    ])

module internal Impl =
  open System.Net.Http

  let endpoint (conf: MixpanelConf) =
    let ub = UriBuilder(conf.endpoint)
    ub.Path <- Track.path
    ub.Uri

  /// Guards so that all sent messages are successfully written.
  let guardRespCode (runtime: RuntimeInfo) (body, statusCode) =
    if statusCode >= 200 && statusCode <= 299 && body = "1" then
      Job.result ()
    else
      runtime.logger.errorWithBP (
        eventX "Mixpanel target received response {statusCode} with {body}."
        >> setField "statusCode" statusCode
        >> setField "body" body)
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
      send: Json list -> Alt<unit> }

    interface IDisposable with
      member x.Dispose() =
        x.client.Dispose()

    static member create (conf: MixpanelConf) (runtime: RuntimeInfo) =
      let client, endpoint = new HttpClient(), endpoint conf

      let create (msgs: Json list) =
        let data =
          Json.format (Json.Array msgs)
          |> UTF8.encodeBase64

        Request.createWithClient client Post endpoint
        |> Request.body (BodyForm [ NameValue ("data", data) ])

      let filters: JobFilter<Request, Response, Json list, unit> =
        codec create bodyAndCode
        >> sinkJob (guardRespCode runtime)

      { client = client; send = filters getResponse }

  let loop (conf: MixpanelConf) (api: TargetAPI) =
    let logger = api.runtime.logger

    logger.info (
      eventX "Started Mixpanel target with endpoint {endpoint}."
      >> setField "endpoint" (endpoint conf))

    let rec loop (state: State): Job<unit> =
      Alt.choose [
        api.shutdownCh ^=> fun ack ->
          logger.verbose (eventX "Shutting down Mixpanel target.")
          ack *<= () :> Job<_>

        RingBuffer.takeBatch conf.batchSize api.requests ^=> fun messages ->
          let entries, acks, flushes =
            messages |> Array.fold (fun (entries, acks, flushes) -> function
              | Log (message, ack) ->
                let nextEntries =
                  match E.message logger conf message with
                  | None -> entries
                  | Some m -> m :: entries
                nextEntries,
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
            do! entries |> state.send
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

/// Create a new Mixpanel target
[<CompiledName "Create">]
let create conf name =
  TargetConf.createSimple (Impl.loop conf) name

/// Use with LogaryFactory.New( s => s.Target<Mixpanel.Builder>() )
type Builder(conf: MixpanelConf, callParent: Target.ParentCallback<Builder>) =
  let update (conf': MixpanelConf): Builder =
    Builder(conf', callParent)

  /// Sets the batch size; how many events to send to Mixpanel in one go.
  member x.BatchSize(batchSize: uint16) =
    Builder({ conf with batchSize = max 1us (min batchSize 50us) }, callParent)

  /// Sets the Mixpanel authentication token.
  member x.Token(token: Token) =
    Builder({ conf with token = token }, callParent)

  /// Sets the Mixpanel API endpoint. Useful for stubbing Mixpanel locally.
  member x.WriteEndpoint(endpoint: string) =
    Builder({ conf with endpoint = endpoint }, callParent)

  /// You've finished configuring the Mixpanel target.
  member x.Done () =
    ! (callParent x)

  new(callParent: Target.ParentCallback<_>) =
    Builder(empty, callParent)

  interface Target.SpecificTargetConf with
    member x.Build name = create conf name
