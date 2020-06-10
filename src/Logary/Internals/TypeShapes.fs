namespace Logary.Internals

open System
open Logary.Internals.TypeShape.Core

// Logary.HashMap

type IHashMapVisitor<'R> =
  abstract Visit<'K, 'V when 'K : equality> : unit -> 'R

type IShapeHashMap =
  abstract Key: TypeShape
  abstract Value: TypeShape
  abstract Accept: IHashMapVisitor<'R> -> 'R

// System.Collections.Generic.IDictionary`2

type IIDictionaryVisitor<'R> =
  abstract Visit<'K, 'V when 'K : equality> : unit -> 'R

type IShapeIDictionary =
  abstract Key: TypeShape
  abstract Value: TypeShape
  abstract Accept: IIDictionaryVisitor<'R> -> 'R

module Shape =
  let private SomeU = Some() // avoid allocating all the time
  let inline private test<'T> (s: TypeShape) =
    match s with
    | :? TypeShape<'T> -> SomeU
    | _ -> None

  // System.Collections.Generic.IDictionary`2

  type private ShapeIDictionary<'K, 'V when 'K : equality> () =
    interface IShapeIDictionary with
      member __.Key = shapeof<'K>
      member __.Value = shapeof<'V>
      member __.Accept v = v.Visit<'K, 'V> ()

  let (|IDictionary|_|) (shape: TypeShape) =
    match shape.ShapeInfo with
    | Generic(td, ta) when td = typedefof<System.Collections.Generic.IDictionary<_, _>> ->
      Activator.CreateInstanceGeneric<ShapeIDictionary<_, _>>(ta)
      :?> IShapeIDictionary
      |> Some
    | _ ->
      None

  // Logary.HashMap`2

  type private ShapeHashMap<'K, 'V when 'K : equality> () =
    interface IShapeHashMap with
      member __.Key = shapeof<'K>
      member __.Value = shapeof<'V>
      member __.Accept v = v.Visit<'K, 'V> ()

  let (|HashMap|_|) (shape: TypeShape) =
    match shape.ShapeInfo with
    // I could not make 'k when 'k :> IEquatable<'k> work with typedefof<HashMap<_,_>>
    | Generic(td, ta) when td.FullName = "Logary.HashMap`2" ->
      Activator.CreateInstanceGeneric<ShapeHashMap<_,_>>(ta)
      :?> IShapeHashMap
      |> Some
    | _ ->
      None

  let (|LogLevel|_|) (shape: TypeShape) = test<Logary.LogLevel> shape
  let (|PointName|_|) (shape: TypeShape) = test<Logary.PointName> shape
  let (|Gauge|_|) (shape: TypeShape) = test<Logary.Gauge> shape
  let (|TraceId|_|) (shape: TypeShape) = test<Logary.Trace.TraceId> shape
  let (|SpanId|_|) (shape: TypeShape) = test<Logary.Trace.SpanId> shape
  let (|Value|_|) (shape: TypeShape) = test<Logary.Value> shape
  let (|Instant|_|) (shape: TypeShape) = test<NodaTime.Instant> shape
  let (|LocalDate|_|) (shape: TypeShape) = test<NodaTime.LocalDate> shape
  let (|Duration|_|) (shape: TypeShape) = test<NodaTime.Duration> shape
  let (|Uri|_|) (shape: TypeShape) = test<System.Uri> shape
  let (|IPAddress|_|) (shape: TypeShape) = test<System.Net.IPAddress> shape
  let (|IPEndPoint|_|) (shape: TypeShape) = test<System.Net.IPEndPoint> shape

  let (|EventMessage|_|) (shape: TypeShape) = test<Logary.EventMessage> shape
  let (|HistogramMessage|_|) (shape: TypeShape) = test<Logary.HistogramMessage> shape
  let (|GaugeMessage|_|) (shape: TypeShape) = test<Logary.GaugeMessage> shape
  let (|SpanMessage|_|) (shape: TypeShape) = test<Logary.SpanMessage> shape
  let (|ControlMessage|_|) (shape: TypeShape) = test<Logary.ControlMessage> shape
