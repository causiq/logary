﻿module Logary.Targets.SerilogTarget

open System
open System.Threading
open Hopac
open Hopac.Infixes
open Logary
open Logary.Target
open Logary.Targets
open Logary.Internals
open Logary.Configuration

type SerilogConf =
  { logger : Serilog.ILogger
    /// Converts a logary level into the corresponding serilog level.
    levelToSerilog: LogLevel -> Serilog.Events.LogEventLevel
    /// Converts a logary PointName into a serilog log event properties.
    nameToSerilog: (PointName -> Serilog.Events.LogEventProperty seq) option
    /// Converts a logary Message into serilog log event properties.
    /// By default, this will 
    messageToSerilog: (SerilogConf -> Message -> Serilog.Events.LogEventProperty seq) }

module internal Impl =
  
  let toSerilogLevel = function
    | Logary.LogLevel.Debug -> Serilog.Events.LogEventLevel.Debug
    | Logary.LogLevel.Error -> Serilog.Events.LogEventLevel.Error
    | Logary.LogLevel.Fatal -> Serilog.Events.LogEventLevel.Fatal
    | Logary.LogLevel.Info -> Serilog.Events.LogEventLevel.Information
    | Logary.LogLevel.Verbose -> Serilog.Events.LogEventLevel.Verbose
    | Logary.LogLevel.Warn -> Serilog.Events.LogEventLevel.Warning

  let extractFirstExceptionOrNull logaryMessage = 
    let exns = Utils.Aether.Lens.getPartialOrElse Message.Lenses.errors_ [] logaryMessage

    let getStringFromMapOrFail exnObjMap fieldName =
      match exnObjMap |> Map.find fieldName with | String m -> m | _ -> failwithf "Couldn't find %s in %A" fieldName exnObjMap

    let getStringFromMapOrDefault defaultIfMissing exnObjMap fieldName =
      match exnObjMap |> Map.tryFind fieldName with | Some (String m) -> m | _ -> defaultIfMissing

    match exns with
    | [] -> Unchecked.defaultof<exn>
    | values ->
      
      let objectValues = values
                         |> List.choose (function | Object v -> Some v | _ -> None)
      null // TODO:

  let rec logaryValueToSerilogProperty (v : Value) : Serilog.Events.LogEventPropertyValue =
    let toScalar x = Serilog.Events.ScalarValue x :> Serilog.Events.LogEventPropertyValue
    match v with
    | Value.Bool b -> toScalar b      
    | Value.Float f -> toScalar f
    | Value.Int64 i -> toScalar i
    | Value.BigInt i -> toScalar i
    | Value.String s -> toScalar s
    | Value.Binary (bytes, _) -> toScalar bytes // TODO: content type? consider this again
    | Value.Object map ->
      let stringOrNull s = match s with String tt -> tt | _ -> null
      let maybeTypeTag = map |> Map.tryPick (fun k v -> if k = "_typeTag" then Some (stringOrNull v) else None)
      let typeTagOrNull = match maybeTypeTag with Some s -> s | None -> null
      let serilogProperties =
        map
        |> Map.filter (fun k _ -> k <> "_typeTag")
        |> Map.toSeq
        |> Seq.map (fun (k, v) -> Serilog.Events.LogEventProperty(k, logaryValueToSerilogProperty v))

      Serilog.Events.StructureValue(serilogProperties, typeTagOrNull)
      :> Serilog.Events.LogEventPropertyValue
    | Fraction (x, y) as f -> toScalar f // TODO:
    | Value.Array items ->
      Serilog.Events.SequenceValue (items |> List.map logaryValueToSerilogProperty)
      :> Serilog.Events.LogEventPropertyValue

  let logaryPointNameToSerilog (pointName : PointName) = seq {
    yield Serilog.Events.LogEventProperty(
      "SourceLoggerName", Serilog.Events.ScalarValue(pointName.ToString())) }

  let defaultMessageToSerilogProperties (conf : SerilogConf) (logaryMessage : Message) =
    let newSerilogProperty name value =
      Serilog.Events.LogEventProperty(name, logaryValueToSerilogProperty value)

    seq {
      if conf.nameToSerilog.IsSome then
        yield! conf.nameToSerilog.Value logaryMessage.name
      
      if not logaryMessage.fields.IsEmpty then
        yield!
          logaryMessage.fields
          |> Map.toSeq
          |> Seq.map (fun (pn, Field (v, u)) -> newSerilogProperty (string pn) v)

      if not logaryMessage.context.IsEmpty then
        yield!
          logaryMessage.context
          |> Map.toSeq
          |> Seq.map (fun (k, v) -> newSerilogProperty k v)
    }

  let logaryToSerilog (conf : SerilogConf) (logaryMessage : Logary.Message) =
    let template = match logaryMessage.value with
                   | Event template -> template
                   | _ -> failwith "logaryToSerilog only supports PointValue.Event"

    Serilog.Events.LogEvent(
      timestamp = DateTimeOffset.ofEpoch logaryMessage.timestamp,
      level = toSerilogLevel logaryMessage.level,
      ``exception`` = extractFirstExceptionOrNull logaryMessage,
      messageTemplate = Serilog.Parsing.MessageTemplateParser().Parse template,
      properties = conf.messageToSerilog conf logaryMessage)

  type State = { logger : Serilog.ILogger }

  let serve (conf : SerilogConf)
            (ri : RuntimeInfo)
            (requests : RingBuffer<_>)
            (shutdown : Ch<_>) =
    if isNull conf.logger then
      failwith "You must configure the Logary SerilogConf with a valid Serilog ILogger instance."

    let rec loop (state : State) : Job<unit> =

      Alt.choose [
        shutdown ^=> fun ack -> job {
          // do! Job.Scheduler.isolate (fun _ -> (state :> IDisposable).Dispose())
          return! ack *<= ()
        }

        RingBuffer.take requests ^=> function
          | Log (msg, ack)  ->
            job {
              let serilogEvent = logaryToSerilog conf msg
              do! Job.Scheduler.isolate (fun _ ->
                state.logger.Write serilogEvent)
              do! ack *<= ()
              return! loop state
            }

          | Flush (ackCh, nack) ->
            job {
              do! Ch.give ackCh () <|> nack
              return! loop state
            }
        ] :> Job<_>

    let state = { State.logger = conf.logger }
    loop state

type SerilogConf with
  static member create (serilogLogger) =
    { logger = serilogLogger
      nameToSerilog = Some Impl.logaryPointNameToSerilog
      levelToSerilog = Impl.toSerilogLevel
      messageToSerilog = Impl.defaultMessageToSerilogProperties }

let empty = SerilogConf.create null

/// Create a new Shipper target
let create conf = TargetUtils.stdNamedTarget (Impl.serve conf)

/// Use with LogaryFactory.New( s => s.Target<Noop.Builder>() )
type Builder(conf, callParent : FactoryApi.ParentCallback<Builder>) =

  new(callParent : FactoryApi.ParentCallback<_>) =
    Builder(empty, callParent)

  interface Logary.Target.FactoryApi.SpecificTargetConf with
    member x.Build name = create conf name
