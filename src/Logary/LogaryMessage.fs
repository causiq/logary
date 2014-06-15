namespace Logary

open FSharp.Actor

open NodaTime

open Logary.Metric
open Logary.Targets

/// The messages that can be sent to the registry to interact with it and its
/// running targets.
type LogaryMessage =
  | GetLogger      of string * Logger ReplyChannel
  | GetGauge       of string * Gauge.GaugeInstance ReplyChannel
  | GetCounter     of string * Counter ReplyChannel
  | GetMeter       of string * Meter ReplyChannel
  | GetHistogram   of string * Histogram ReplyChannel
  | GetTimer       of string * Timer ReplyChannel
  | GetHealthCheck of string * HealthCheck ReplyChannel
  | FlushPending   of Duration * Acks ReplyChannel
  | ShutdownLogary of Duration * Acks ReplyChannel
