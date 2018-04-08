/// You must acquire a commercial license from henrik@haf.se to use this code.
module Logary.Targets.OpsGenie

open System
open System.Text
open Hopac
open Hopac.Infixes
open HttpFs.Client
open HttpFs.Composition
open Logary
open Logary.Message
open Logary.Internals
open Logary.Internals.Chiron
open Logary.Configuration

/// An API key
type ApiKey = string

type Responder =
  | Team of teamId:string
  | User of userId:string
  | Escalation of eid:string
  | Schedule of sid:string

/// https://docs.opsgenie.com/docs/authentication
/// The configuration record for OpsGenie communication.
type OpsGenieConf =
  { endpoint: string
    /// Authentication token used for accessing the API.
    apiKey: ApiKey
    /// Alias the message to something semantically useful.
    getAlias: Message -> string
    getResponders: Message -> Responder[] }

type Priority =
  | P1
  | P2
  | P3
  | P4
  | P5
  static member ofLogLevel = function
    | Verbose -> P5
    | Debug -> P5
    | Info -> P4
    | Warn -> P3
    | Error -> P2
    | Fatal -> P1

module internal E =
  open Logary.Message.Patterns
  open Logary.Formatting

  module E = Json.Encode

  let private limitTo (max: int) (s: string) =
    if s.Length > max then s.Substring(0, max) else s

  let message (value: string) =
    E.string (limitTo 130 value)

  let alias (messageAlias: string) =
    E.string (limitTo 512 messageAlias)

  let description (d: string) =
    E.string (limitTo 15000 d)

  let responder =
    E.propertyList << function
    | Team teamId ->
      [
        "type", String "team"
        "id", String teamId
      ]
    | User userId ->
      [
        "type", String "user"
        "id", String userId
      ]
    | Escalation eid ->
      [
        "type", String "escalation"
        "id", String eid
      ]
    | Schedule sid ->
      [
        "type", String "schedule"
        "id", String sid
      ]

  let responders =
    E.arrayWith responder

//  let visibleTo
// Custom actions that will be available for the alert.
//  let actions
  let tags (ts: Set<string>) =
    E.stringSet ts

  let details (values: seq<string * obj>) =
    values
    // flatten the object, or else, OpsGenie barfs
    // [14:32:08 ERR] OpsGenie target received response 422 with "'details#tenant' is invalid"
    |> Seq.map (fun (k, v) -> k, Json.String (string v))
    |> Seq.toList
    |> JsonObject.ofPropertyList
    |> Json.Object

  let entity (e: string option) =
    e |> Option.map E.string |> E.option

  let source (logger: PointName) =
    logger
    |> PointName.format
    |> limitTo 100
    |> E.string

  let priority p =
    E.string <|
    match p with
    | P1 -> "P1"
    | P2 -> "P2"
    | P3 -> "P3"
    | P4 -> "P4"
    | P5 -> "P5"

  let user (u: string option) =
    u |> Option.map (limitTo 100) |> Option.map E.string |> E.option

  /// Formats all exceptions as a big note.
  let private exnsToString (m: Message) =
    match Message.getExns m with
    | [] ->
      None
    | es ->
      es
      |> List.map (sprintf "%O")
      |> String.concat "\n\n"
      |> limitTo 25000
      |> Some

  let private getDescription m =
    match Message.tryGetField "description" m, exnsToString m with
    | None, None ->
      None
    | Some desc, Some exnText ->
      Some (sprintf "%s\n\n%s" desc exnText)
    | None, Some exnText ->
      Some exnText
    | Some desc, None ->
      Some desc

  let logaryMessage (conf: OpsGenieConf) (x: Message) jObj =
    jObj
    |> E.required message "message" x.value
    |> E.required alias "alias" (conf.getAlias x)
    |> E.optional description "description" (getDescription x)
    |> E.required responders "responders" (conf.getResponders x)
    |> E.required tags "tags" (Message.getAllTags x)
    |> E.required details "details" (Message.getAllFields x)
    |> E.required source "source" x.name
    |> E.required (Priority.ofLogLevel >> priority) "priority" x.level

  let encode (conf: OpsGenieConf) (m: Message): Json option =
    if String.IsNullOrWhiteSpace m.value then None else
    Some (logaryMessage conf m JsonObject.empty |> Json.Object)

module Alias =
  open Logary.Formatting
  /// # of exceptions:
  ///  - 1: The formatted message value alone, if no exceptions.
  ///
  /// Otherwise, the message value/template and
  let defaultAlias (m: Message) =
    let formatted = MessageWriter.verbatim.format m
    match Message.getExns m with
    | [] ->
      formatted
    | e :: [] ->
      sprintf "%s: %s" formatted e.Message
    | es ->
      sprintf "%s (multiple exns); { %s }"
        formatted
        (es |> List.map (fun e -> sprintf "'%s'" e.Message) |> String.concat ", ")

[<AutoOpen>]
module OpsGenieConfEx =
  type OpsGenieConf with
    /// Create a new OpsGenie configuration record for Logary to use for sending
    /// events to OpsGenie with.
    /// Optional:
    /// - `getAlias`: dedup-name for the message, should be human readable
    /// - `getResponders`: special-case responding. Perhaps by the assembly that triggered
    ///   the message?
    static member create (apiKey, ?endpoint, ?getAlias, ?getResponders) =
      { endpoint = defaultArg endpoint "https://api.opsgenie.com/v2"
        apiKey = apiKey
        getAlias = defaultArg getAlias Alias.defaultAlias
        getResponders = defaultArg getResponders (fun _ -> Array.empty) }

let empty =
  OpsGenieConf.create (
    Env.varDefault "OPSGENIE_API_KEY" (fun () -> "OPSGENIE_API_KEY=MISSING"))

module internal Impl =
  open Logary.Internals.Aether
  open Logary.Internals.Aether.Operators
  open Chiron.Optics
  open System.Net.Http

  let endpoint (conf: OpsGenieConf) =
    let ub = UriBuilder(conf.endpoint)
    ub.Path <- sprintf "%s/alerts" ub.Path
    ub.Uri

  let private errorMessage_ =
     Optics.compose (Optics.compose Json.Object_ (Optics.JsonObject.key_ "message")) Optics.Json.String_

  /// Guards so that all sent messages are successfully written.
  let guardRespCode (runtime: RuntimeInfo) (body: Json, statusCode) =
    /// Create alert requests are processed asynchronously, therefore valid requests are responded with HTTP status 202 - Accepted.
    if statusCode = 202 then
      Job.result ()
    else
      let message = Optics.get errorMessage_ body |> JsonResult.getOrThrow
      runtime.logger.logWithAck Error (
        eventX "OpsGenie target received response {statusCode} with {message}."
        >> setField "statusCode" statusCode
        >> setField "message" message)
      |> Job.bind id
      |> Job.bind (fun () -> Job.raises (Exception message))

  let bodyAndCode (resp: Response) =
    resp
    |> Job.useIn Response.readBodyAsString
    |> Job.map (Json.parse >> JsonResult.getOrThrow)
    |> Job.map (fun body -> body, resp.statusCode)

  let speakJson: JobFilter<Request, _> =
    let ct = ContentType.create ("application", "json", Encoding.UTF8)
    fun next req ->
      req
      |> Request.setHeader (RequestHeader.Accept "application/json")
      |> Request.setHeader (RequestHeader.ContentType ct)
      |> next

  let authenticated (apiKey: ApiKey): JobFilter<Request, _> =
    fun next req ->
      req
      |> Request.setHeader (RequestHeader.Authorization (sprintf "GenieKey %s" apiKey))
      |> next

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
      send: Json -> Alt<unit> }

    interface IDisposable with
      member x.Dispose() =
        x.client.Dispose()

    static member create (conf: OpsGenieConf) (runtime: RuntimeInfo) =
      let client, endpoint = new HttpClient(), endpoint conf

      let create (msg: Json) =
        Request.createWithClient client Post endpoint
        |> Request.bodyString (Json.format msg)

      let filters: JobFilter<Request, Response, Json, unit> =
        speakJson
        >> authenticated conf.apiKey
        >> codec create bodyAndCode
        >> sinkJob (guardRespCode runtime)

      { client = client; send = filters getResponse }

  let loop (conf: OpsGenieConf) (runtime: RuntimeInfo, api: TargetAPI) =
    runtime.logger.info (
      eventX "Started OpsGenie target with endpoint {endpoint}."
      >> setField "endpoint" (endpoint conf))

    let rec loop (state: State): Job<unit> =
      Alt.choose [
        api.shutdownCh ^=> fun ack ->
          runtime.logger.verbose (eventX "Shutting down OpsGenie target.")
          ack *<= () :> Job<_>

        RingBuffer.take api.requests ^=> function
          | Log (message, ack) ->
            job {
              do runtime.logger.verbose (eventX "Writing an alert")
              do! E.encode conf message
                  |> Option.map state.send
                  |> Option.orDefault (fun () -> Alt.always ())
              do runtime.logger.verbose (eventX "Acking messages")
              do! ack *<= ()
              return! loop state
            }

          | Flush (ackCh, nack) ->
            ackCh *<= ()

      ] :> Job<_>

    job {
      use state = State.create conf runtime
      return! loop state
    }

/// Create a new Mixpanel target
[<CompiledName "Create">]
let create conf name =
  TargetConf.createSimple (Impl.loop conf) name

/// Use with LogaryFactory.New( s => s.Target<Mixpanel.Builder>() )
type Builder(conf: OpsGenieConf, callParent: Target.ParentCallback<Builder>) =
  let update (conf': OpsGenieConf): Builder =
    Builder(conf', callParent)

  /// Sets the Mixpanel authentication token.
  member x.Token(apiKey: ApiKey) =
    Builder({ conf with apiKey = apiKey }, callParent)

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

