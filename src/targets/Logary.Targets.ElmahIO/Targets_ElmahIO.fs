module Logary.Targets.ElmahIO

open System
open NodaTime
open Hopac
open Hopac.Infixes
open Logary
open Logary.Message
open Logary.Target
open Logary.Internals
open Elmah.Io.Client

/// Configuration for the Elmah.IO target, see
/// https://github.com/elmahio/elmah.io and
/// https://elmah.io/
type ElmahIOConf =
  { /// You can get the log id from the https://elmah.io/dashboard/ page by clicking
    /// on the settings icon and then taking the logId from the tutorial.
    logId : Guid }

let empty = { logId = Guid.Empty }

module internal Impl =
  open Logary.Message.Lenses
  open Logary.Internals.Aether
  open Logary.Internals.Aether.Operators

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
      |> Seq.collect (fun (KeyValue (k, v)) ->
        Formatting.MessageParts.formatValueLeafs [k] v
      )
      message.fields
      |> Seq.collect (fun (KeyValue (k, (Field (value, units)))) ->
        let root = PointName.format k
        Formatting.MessageParts.formatValueLeafs [root] value
      )
    ]
    |> Seq.map (fun (k, v) ->
      Elmah.Io.Client.Item(
        Key = PointName.format k,
        Value = v
      ))
    |> fun xs -> Collections.Generic.List<_>(xs)

  let firstError_ =
    errors_
    >??> head_

  let firstErrorType_ =
    firstError_
    >??> Value.Object_
    >??> key_ "type"
    >??> Value.String_

  let getType (message : Logary.Message) =
    match Lens.getPartial firstErrorType_ message with
    | Some typ ->
      typ

    | None ->
      PointName.format message.name

  let tryGet (message : Logary.Message) (key : string) : string option =
    Lens.getPartial (field_ key) message
    |> Option.bind (function
    | Field (value, units) ->
      let (PointName parts) = PointName.parse key
      let formatted =
        Formatting.MessageParts.formatValueLeafs (List.ofArray parts) value
      formatted
      |> Seq.tryHead
      |> Option.map snd)

  let tryGetInt (message : Logary.Message) (key : string) : int option = 
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

  let format (message : Logary.Message) =
    Formatting.StringFormatter.levelDatetimeMessagePath.format message

  type State =
    { logger : ILogger }
    interface IDisposable with
      member x.Dispose () = ()

  let loop (conf : ElmahIOConf)
           (ri : RuntimeInfo)
           (requests : RingBuffer<_>)
           (shutdown : Ch<_>) =

    let internalError = Ch ()

    let rec loop (state : State) : Job<unit> =
      Alt.choose [
        internalError ^=> fun (args : FailEventArgs) ->
          let message, ex = args.Message, args.Error
          Message.eventError message.Title
          |> Message.setFieldFromObject "message" message
          |> Message.addExn ex
          |> Logger.logSimple ri.logger
          loop state

        RingBuffer.take requests ^=> function
          | Log (Event template as message, ack) ->
            let elmahMessage =
              Elmah.Io.Client.Message(template,
                Id = (match tryGet message "messageId" with None -> null | Some x -> x),
                Application = ri.service,
                Detail = format message,
                Source = PointName.format message.name,
                StatusCode = Option.toNullable (tryGetInt message "statusCode"),
                DateTime = Instant.FromTicksSinceUnixEpoch(message.timestampTicks).ToDateTimeUtc(),
                Type = getType message,
                User = (match tryGet message "user.name" with None -> null | Some x -> x),
                Severity = Nullable<_>(Severity.ofLogLevel message.level),
                Url = (match tryGet message "url" with None -> null | Some x -> x),
                //Method -> new property in master,
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
            ri.logger.verboseWithBP (eventX "Logging of metrics not supported")
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

    let state =
      { logger = new Logger(conf.logId) // Elmah.IO specific
      }

    state.logger.OnMessageFail.Add (Ch.send internalError >> start)

    loop state

/// Create a new Elmah.IO target
let create conf =
  if conf.logId = Guid.Empty then
    failwith "Cannot configure target with empty logId"

  TargetUtils.stdNamedTarget (Impl.loop conf)

/// Use with LogaryFactory.New( s => s.Target<ElmahIO.Builder>().WithLogId("MY GUID HERE") )
type Builder(conf, callParent : FactoryApi.ParentCallback<Builder>) =
  member x.WithLogId(logId : Guid) =
    ! (callParent <| Builder({ conf with logId = logId }, callParent))

  new(callParent : FactoryApi.ParentCallback<_>) =
    Builder({ logId = Guid.Empty }, callParent)

  interface Logary.Target.FactoryApi.SpecificTargetConf with
    member x.Build name = create conf name