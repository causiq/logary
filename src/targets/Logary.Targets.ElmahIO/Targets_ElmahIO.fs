module Logary.Targets.ElmahIO

// Ignore deprecations — if this target breaks it's removed from the code base
#nowarn "44"

open System
open NodaTime
open Hopac
open Hopac.Infixes
open Logary
open Logary.Message
open Logary.Internals
open Logary.Configuration
open Elmah.Io.Client
open System.Reflection

/// Configuration for the Elmah.IO target, see
/// https://github.com/elmahio/elmah.io and
/// https://elmah.io/
type ElmahIOConf =
  { /// You can get the log id from the https://elmah.io/dashboard/ page by clicking
    /// on the settings icon and then taking the logId from the tutorial.
    logId: Guid
    apiKey: string}

let empty = { logId = Guid.Empty ; apiKey = String.Empty}

module internal Impl =
  open Logary.Internals.Aether

  module Severity =

    let ofLogLevel = function
      | LogLevel.Verbose -> Severity.Verbose
      | LogLevel.Debug -> Severity.Debug
      | LogLevel.Info -> Severity.Information
      | LogLevel.Warn -> Severity.Warning
      | LogLevel.Error -> Severity.Error
      | LogLevel.Fatal -> Severity.Fatal


  let getVersion () =
    let a = System.Reflection.Assembly.GetCallingAssembly()
    System.Reflection.Assembly.GetCallingAssembly()
      .GetCustomAttribute<AssemblyInformationalVersionAttribute>()
      .InformationalVersion

  let getData (msg: Logary.Message) =
    msg.context
    |> HashMap.toSeq
    |> Seq.map( fun (name, v) ->
       let jsonStr = Logary.Formatting.Json.format v
       Elmah.Io.Client.Models.Item(name, jsonStr))
    |> fun xs -> Collections.Generic.List<_>(xs)

  let getType (msg: Logary.Message) =
    match msg |> getExns |>  List.tryLast with
    | Some exn ->
      exn.GetType().FullName
    | None ->
      PointName.format msg.name

  let format =
    Logary.MessageWriter.levelDatetimeMessagePath.format

  type State =
    { client: IElmahioAPI }
    interface IDisposable with
      member x.Dispose () =
        x.client.Dispose ()

  let loop (conf: ElmahIOConf) (api: TargetAPI) =
    let internalError = Ch ()
    
    let rec initialise (): Job<unit> =
      if conf.logId = Guid.Empty || String.IsNullOrEmpty(conf.apiKey) then
        Job.raises (exn "Cannot configure target with empty logId or empty apiKey")
      else
        let state = { client = ElmahioAPI.Create(conf.apiKey) } // Elmah.IO specific
        state.client.Messages.OnMessageFail.Add (Ch.send internalError >> start)
        running state

    and running (state: State): Job<unit> =
      Alt.choose [
        internalError ^=> fun (args: FailEventArgs) ->
          let message, ex = args.Message, args.Error
          Message.eventError message.Title
          |> Message.setContext "internalErrorMessage" message
          |> Message.addExn ex
          |> Logger.logSimple api.runtime.logger
          running state

        RingBuffer.take api.requests ^=> function
          | Log (msg, ack) ->
            let sendMsg () =
              let elmahMsg = Elmah.Io.Client.Models.CreateMessage()
              elmahMsg.Application <- api.runtime.service
              elmahMsg.Hostname <- api.runtime.host
              elmahMsg.Detail <- format msg
              elmahMsg.DateTime <- Nullable(Instant.FromUnixTimeTicks(msg.timestampTicks).ToDateTimeUtc())
              elmahMsg.Title <- Logary.MessageWriter.verbatim.format msg
              elmahMsg.Severity <- msg.level |> Severity.ofLogLevel |> string
              elmahMsg.Source <- PointName.format msg.name
              elmahMsg.Type <- getType msg
              // elmahMsg.Cookies <- ""
              elmahMsg.Data <- getData msg
              // elmahMsg.Form <- ""
              // elmahMsg.Method <- ""
              // elmahMsg.QueryString <- ""
              // elmahMsg.ServerVariables <- ""
              elmahMsg.StatusCode <- Option.toNullable (msg |> tryGetContext "statusCode")
              elmahMsg.Url <- match tryGetContext "url" msg with None -> null | Some x -> x
              elmahMsg.Version <- getVersion()
              elmahMsg.User <- match (tryGetContext "user.name" msg) with | None -> null | Some x -> x

              state.client.Messages.CreateAndNotifyAsync(conf.logId, elmahMsg)

            Job.fromTask sendMsg
            >>= fun _ -> ack *<= ()
            >>=. running state

          | Flush (ackCh, nack) ->
            job {
              do! IVar.fill ackCh ()
              return! running state
            }

        api.shutdownCh ^=> fun ack ->
          Job.Scheduler.isolate (fun _ -> (state :> IDisposable).Dispose())
          >>=. ack *<= ()
          
      ] :> Job<_>

    initialise ()

/// Create a new Elmah.IO target
[<CompiledName "Create">] 
let create conf name: TargetConf =
  TargetConf.createSimple (Impl.loop conf) name

/// Use with LogaryFactory.New( s => s.Target<ElmahIO.Builder>().WithLogId("MY GUID HERE") )
type Builder(conf, callParent: Target.ParentCallback<Builder>) =
  member x.WithLogIdAndApiKey(logId: Guid, apiKey: string) =
    ! (callParent <| Builder({ conf with logId = logId ; apiKey = apiKey}, callParent))

  new(callParent: Target.ParentCallback<_>) =
    Builder(empty, callParent)

  interface Target.SpecificTargetConf with
    member x.Build name = create conf name