namespace Logary.Targets

open System
open System.Collections.Generic
open Confluent.Kafka
open Logary
open Logary.Model

type KafkaTargetExn(message, error: Error) =
  inherit Exception(message)
  member x.error = error

  static member ofError (e: Error) =
    KafkaTargetExn(sprintf "Publish failed %s with code=%O" (if e.IsFatal then "fatally" else "non-fatally") e.Code,
                   e)

  static member toKeyValues (baseKey, error: Error) =
    [ "error", Value.Bool true
      "errorCode", Value.Str (error.Code.ToString())
      "errorReason", Value.Str error.Reason
      "isBrokerError", Value.Bool error.IsBrokerError
      "isLocalError", Value.Bool error.IsLocalError ]
    |> List.map (fun (k, v) -> sprintf "%s.%s" baseKey k, v)

  static member toKeyValues (baseKey, report: DeliveryReport<Id, LogaryMessageBase>) =
    KafkaTargetExn.toKeyValues(baseKey, report.Error)

  static member toRODict (baseKey, error: Error) =
    KafkaTargetExn.toKeyValues (baseKey, error)
      |> Map
      :> IReadOnlyDictionary<string, Value>

  static member toRODict (baseKey, report: DeliveryReport<Id, LogaryMessageBase>) =
    KafkaTargetExn.toKeyValues (baseKey, report)
      |> Map
      :> IReadOnlyDictionary<string, Value>

  interface IValueFormattable with
    member x.toKeyValues baseKey =
      KafkaTargetExn.toRODict(baseKey, error)
        |> Choice2Of2

