module Logary.Targets.ElmahIO

open System
open NodaTime
open Hopac
open Hopac.Infixes
open Logary
open Logary.Target
open Logary.Internals
open Elmah.Io.Client

/// Configuration for the Elmah.IO target, see
/// https://github.com/elmahio/elmah.io and
/// https://elmah.io/
type ElmahIOConf =
  { /// You can get the log id from the https://elmah.io/dashboard/ page by clicking
    /// on it and then taking the logId from the address bar.
    logId : Guid }

module internal Impl =
  open Logary.Message.Lenses
  open Logary.Utils.Aether
  open Logary.Utils.Aether.Operators

  module Severity =

    let ofLogLevel = function
      | LogLevel.Verbose -> Severity.Verbose
      | LogLevel.Debug -> Severity.Debug
      | LogLevel.Info -> Severity.Information
      | LogLevel.Warn -> Severity.Warning
      | LogLevel.Error -> Severity.Error
      | LogLevel.Fatal -> Severity.Fatal

  let getData (message : Logary.Message) =
    Seq.concat [
      message.context
      |> Seq.map (fun (KeyValue (k, v)) ->
        k, Units.formatValue v
      )
      message.fields
      |> Seq.map (fun (KeyValue (k, Field (v, u))) ->
        PointName.format k, Units.formatWithUnit Units.UnitOrientation.Suffix u v
      )
    ]
    |> Seq.map (fun (k, v) -> Elmah.Io.Client.Item(Key = k, Value = v))
    |> fun xs -> Collections.Generic.List<_>(xs)

  let getType (message : Logary.Message) =
    match Lens.getPartial errors_ message with
    | Some errors ->
      "errors"

    | None ->
      PointName.format message.name

  let inline tryGet (message : Logary.Message) (key : string) : string option =
    match Lens.getPartial (field_ key) message with
    | Some (Field (value, units)) ->
      Some (Units.formatWithUnit Units.UnitOrientation.Suffix units value)
    | None ->
      None

  let inline tryGetInt (message : Logary.Message) (key : string) : int option = 
    match Lens.getPartial (field_ key) message with
    | Some (Field (value, units)) ->
      match Int32.TryParse (Units.formatValue value) with
      | false, _ ->
        None
      | true, value ->
        Some value
    | None ->
      None

  let (|Event|Other|) (m : Logary.Message) =
    match m.value with
    | Event template ->
      Choice1Of2 template
    | _ ->
      Choice2Of2 ()

  let sendMessage (logger : ILogger) (m : Elmah.Io.Client.Message) =
    Job.fromBeginEnd (fun (cb, o) -> logger.BeginLog(m, cb, o)) logger.EndLog

  type State =
    { logger : ILogger }
    interface IDisposable with
      member x.Dispose () = ()

  let loop (conf : ElmahIOConf)
           (ri : RuntimeInfo)
           (requests : RingBuffer<_>)
           (shutdown : Ch<_>) =

    let rec loop (state : State) : Job<unit> =
      Alt.choose [
        RingBuffer.take requests ^=> function
          | Log (Event template as message, ack) ->
            let formatted = Formatting.StringFormatter.levelDatetimeMessagePath.format message
            let elmahMessage =
              Elmah.Io.Client.Message(template,
                Id = (match tryGet message "messageId" with None -> null | Some x -> x),
                Application = ri.serviceName,
                Detail = formatted,
                Source = PointName.format message.name,
                StatusCode = Option.toNullable (tryGetInt message "statusCode"),
                DateTime = Instant.FromTicksSinceUnixEpoch(message.timestampTicks).ToDateTimeUtc(),
                Type = getType message,
                User = (match tryGet message "user.name" with None -> null | Some x -> x),
                Severity = Nullable<_>(Severity.ofLogLevel message.level),
                Url = (match tryGet message "url" with None -> null | Some x -> x),
                //Method = tryGetContext "method",
                Version = Logary.YoLo.App.getVersion(),
                //ServerVariables, // too sensitive to ship
                //QueryString, // too sensitive to ship
                //Form, // too sensitive to ship
                Data = getData message
              )
            job {
              let! txId = sendMessage state.logger elmahMessage
              do! ack *<= ()
              return! loop state
            }

          | Log (message, ack) ->
            Logger.logVerbose ri.logger (
              Message.eventX "Logging of metrics not supported"
            )
            >>=. (ack *<= ())
            >>=. loop state
            

          | Flush (ackCh, nack) ->
            job {
              do! Ch.give ackCh () <|> nack
              return! loop state
            }

        shutdown ^=> fun ack ->
          Job.Scheduler.isolate (fun _ -> (state :> IDisposable).Dispose())
          >>=. ack *<= ()
      ] :> Job<_>

(*     let err = Error ex
      err.ApplicationName <- ri.serviceName
      err.Time <- l.timestamp.ToDateTimeUtc()
      let! entryId = Async.FromBeginEnd(err, state.log.BeginLog, state.log.EndLog) *)

    loop { logger = new Logger(conf.logId) }

/// Create a new Elmah.IO target
let create conf = TargetUtils.stdNamedTarget (Impl.loop conf)

/// C# interop: Create a new Elmah.IO target
[<CompiledName "Create">]
let create' (conf, name) =
  create conf name

/// Use with LogaryFactory.New( s => s.Target<ElmahIO.Builder>().WithLogId("MY GUID HERE") )
type Builder(conf, callParent : FactoryApi.ParentCallback<Builder>) =
  member x.WithLogId(logId : Guid) =
    ! (callParent <| Builder({ conf with logId = logId }, callParent))

  new(callParent : FactoryApi.ParentCallback<_>) =
    Builder({ logId = Guid.Empty }, callParent)

  interface Logary.Target.FactoryApi.SpecificTargetConf with
    member x.Build name = create conf name
