namespace Serilog.Appender

open System
open NodaTime
open Serilog
open Serilog.Events
open Logary

module SerilogEvent =
  /// Map the Serilog.Events.LogEventLevel to a Logary.LogLevel.
  let mapLogLevel level =
    match level with
    | l when l = LogEventLevel.Fatal        -> LogLevel.Fatal
    | l when l = LogEventLevel.Error        -> LogLevel.Error
    | l when l = LogEventLevel.Warning      -> LogLevel.Warn
    | l when l = LogEventLevel.Information  -> LogLevel.Info
    | l when l = LogEventLevel.Debug        -> LogLevel.Debug
    | l when l = LogEventLevel.Verbose      -> LogLevel.Verbose
    | _ -> LogLevel.Info

  let logEventPropertyValueToObj (lep : LogEventPropertyValue) =
    match lep with
    | :? ScalarValue as sv -> sv.Value
    // | :? StructureValue as stv -> box (stv.Properties |> Seq.map (fun p -> p.Value))
    | _ -> box lep

  let toLogary (event : Serilog.Events.LogEvent) =
    let fields =
      event.Properties
      |> Seq.map (fun lep -> lep.Key, logEventPropertyValueToObj lep.Value)
      |> Map.ofSeq

    Message.event (mapLogLevel event.Level) (event.MessageTemplate.Text)
    |> Message.setUTCTicks event.Timestamp.UtcTicks
    |> (Option.ofObj event.Exception
        |> Option.fold (fun s t -> Message.addExn t) id)
    |> Message.setFieldsFromMap fields

type LogarySerilogSink(logaryLogger : Logger) =
  interface Serilog.Core.ILogEventSink with
    member x.Emit (event: Serilog.Events.LogEvent) =
      event
      |> SerilogEvent.toLogary
      |> Logger.log logaryLogger
      |> Hopac.TopLevel.queue
