/// You must acquire a commercial license from henrik@haf.se to use this code.
module Logary.Targets.Mixpanel

open System
open System.Text
open Hopac
open Hopac.Infixes
open Logary
open Logary.Message
open Logary.Internals
open Logary.Internals.Chiron
open Logary.Configuration
open HttpFs.Client
open HttpFs.Composition

/// An API access token
type Token = string

/// The configuration record for Mixpanel communication.
type MixpanelConf =
  { batchSize: uint16
    /// The write endpoint to send the values to
    endpoint: string
    /// Authentication token used for accessing the API.
    token: Token }
  /// Create a new Mixpanel configuration record for Logary to use for sending
  /// events to Mixpanel with.
  static member create (token, ?endpoint, ?batchSize) =
    { batchSize = max 1us (min (defaultArg batchSize 50us) 50us)
      endpoint = defaultArg endpoint "https://api.mixpanel.com/"
      token = token }

module Track =
  let path = "/track"
module Engage =
  let path = "/engage"

let empty = MixpanelConf.create "MISSING_MIXPANEL_TOKEN"

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
      runtime.logger.logWithAck Error (
        eventX "Mixpanel target received response {statusCode} with {body}."
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

    static member create (conf: MixpanelConf) (runtime: RuntimeInfo) =
      let client, endpoint = new HttpClient(), endpoint conf

      let create body =
        Request.createWithClient client Post endpoint
        |> Request.bodyString body

      let filters: JobFilter<Request, Response, string, unit> =
        codec create bodyAndCode
        >> sinkJob (guardRespCode runtime)

      { client = client; send = filters getResponse }

  let loop (conf: MixpanelConf) (runtime: RuntimeInfo, api: TargetAPI) =
    runtime.logger.info (
      eventX "Started Mixpanel target with endpoint {endpoint}."
      >> setField "endpoint" (endpoint conf))

    let rec loop (state: State): Job<unit> =
      Alt.choose [
        api.shutdownCh ^=> fun ack ->
          runtime.logger.verbose (eventX "Shutting down Mixpanel target.")
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
