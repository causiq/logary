[<AutoOpen>]
module Logary.ModelEx

open Logary
open Logary.Internals
open Logary.Model
open Logary.Trace
open NodaTime

type Logary.LogaryMessage with
  /// Definitely gets the message as the mutable base type; if the user has logged own implementations of the interfaces
  /// they are now replaced with the Logary-specified implementations.
  member x.getAsBase(): LogaryMessageBase =
    match x with
    | :? LogaryMessageBase as b -> b
    | :? Logary.ControlMessage as c -> Model.ControlMessage c :> _
    | :? Logary.EventMessage as e -> Model.Event e :> _
    | :? Logary.GaugeMessage as g -> Model.GaugeMessage g :> _
    | :? Logary.HistogramMessage as h -> Model.HistogramMessage h :> _
    | :? Logary.SpanMessage as s -> Model.SpanMessage s :> _
    | :? Logary.IdentifyUserMessage as m -> Model.IdentifyUserMessage m :> _
    | :? Logary.SetUserPropertyMessage as m -> Model.SetUserPropertyMessage m :> _
    // TO CONSIDER: remember to add more subtypes here as you add them
    | _ ->
      let t = x.GetType()
      let interfaces = t.GetInterfaces() |> Seq.map (fun i -> i.FullName) |> String.concat " ,"
      failwithf "Unexpected implementation type '%s' of Logary.LogaryMessage; ensure ModelEx.fs:11 has a case for the new interfaces implemented by this type: { %s }"
                t.FullName interfaces

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
  member x.setField(baseKey, formattable: IValueFormattable) =
    match formattable.toKeyValues baseKey with
    | Choice1Of2 (KeyValue (key, value)) ->
      x.setField(key, value)
    | Choice2Of2 kvs ->
      x.setFieldValues kvs

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
  member x.setContext(baseKey, formattable: IValueFormattable) =
    match formattable.toKeyValues baseKey with
    | Choice1Of2 (KeyValue (key, value)) ->
      x.setContext(key, value)
    | Choice2Of2 kvs ->
      x.setContextValues kvs

  member x.tag tag =
    x.setField(tag, Value.Bool true)
  member x.untag tag =
    x.fields.Remove tag
      |> ignore
  member x.hasTag tag =
    match x.fields.TryGetValue tag with
    | true, Value.Bool b when b -> true
    | _ -> false

  /// Warning: O(n) operation; instead do a `x.hasTag "my tag"` check, if you can.
  member x.getAllTags () =
    seq {
      for KeyValue (k, v) in x.fields do
        match k, v with
        | _, Value.Bool b when b ->
          yield k
        | _ -> ()
    }

type Logary.Gauge with
  static member timeEvent event fn =
    let timedFn = Gauge.time fn
    fun input ->
      let ts = MonotonicClock.getTimestamp()
      let g, res = timedFn input
      let m = Model.Event(event, None, timestamp=ts)
      m.setGauge("duration", g)
      m, res

  static member timeGauge labels fn =
    let timedFn = Gauge.time fn
    fun input ->
      let ts = MonotonicClock.getTimestamp()
      let g, res = timedFn input
      let m = Model.GaugeMessage(g, labels, timestamp=ts)
      m, res
