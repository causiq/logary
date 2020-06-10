[<AutoOpen>]
module Logary.ModelEx

open Logary
open Logary.Model
open Logary.Trace
open NodaTime

type Logary.LogaryMessage with
  member x.getAsBase(onMatchFailure: Logary.LogaryMessage -> #LogaryMessageBase): LogaryMessageBase =
    match x with
    | :? LogaryMessageBase as b -> b
    | _ -> onMatchFailure x :> _
    // Remember to add subtypes here when you build them.

type Model.LogaryMessageBase with
  member x.setField(key, value: float) = x.setField(key, Value.ofPrimitive value)
  member x.setField(key, value: int16) = x.setField(key, Value.ofPrimitive value)
  member x.setField(key, value: uint16) = x.setField(key, Value.ofPrimitive value)
  member x.setField(key, value: int32) = x.setField(key, Value.ofPrimitive value)
  member x.setField(key, value: uint32) = x.setField(key, Value.ofPrimitive value)
  member x.setField(key, value: int64) = x.setField(key, Value.ofPrimitive value)
  member x.setField(key, value: uint64) = x.setField(key, Value.ofPrimitive value)
  member x.setField(key, value: bigint) = x.setField(key, Value.ofPrimitive value)
  member x.setField(key, numerator: int64, denominator: int64) = x.setField(key, Value.Fraction (numerator, denominator))
  member x.setField(key, value: string) = x.setField(key, Value.ofPrimitive value)
  member x.setField(key, value: bool) = x.setField(key, Value.ofPrimitive value)
  member x.setField(key, value: SpanId) = x.setField(key, Value.ofPrimitive value)
  member x.setField(key, value: TraceId) = x.setField(key, Value.ofPrimitive value)

  member x.setGauge(key, value: Duration) = x.setGauge(key, Gauge.ofDuration value)

  member x.setContext(key, value: float) = x.setContext(key, Value.ofPrimitive value)
  member x.setContext(key, value: int16) = x.setContext(key, Value.ofPrimitive value)
  member x.setContext(key, value: uint16) = x.setContext(key, Value.ofPrimitive value)
  member x.setContext(key, value: int32) = x.setContext(key, Value.ofPrimitive value)
  member x.setContext(key, value: uint32) = x.setContext(key, Value.ofPrimitive value)
  member x.setContext(key, value: int64) = x.setContext(key, Value.ofPrimitive value)
  member x.setContext(key, value: uint64) = x.setContext(key, Value.ofPrimitive value)
  member x.setContext(key, value: bigint) = x.setContext(key, Value.ofPrimitive value)
  member x.setContext(key, numerator: int64, denominator: int64) = x.setContext(key, Value.Fraction (numerator, denominator))
  member x.setContext(key, value: string) = x.setContext(key, Value.ofPrimitive value)
  member x.setContext(key, value: bool) = x.setContext(key, Value.ofPrimitive value)
