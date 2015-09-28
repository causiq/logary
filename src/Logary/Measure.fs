namespace Logary

open FSharp.Actor
open NodaTime

open Logary.DataModel

type TimeUnit =
  | Nanoseconds
  /// Ticks as defined by `NodaTime.NodaConstants.TicksPerSecond`.
  | Ticks
  | Microseconds
  | Milliseconds
  | Seconds
  | Minutes
  | Hours
  | Days
with
  override x.ToString() =
    match x with
    | Nanoseconds  -> "ns"
    | Ticks        -> "ticks"
    | Microseconds -> "µs"
    | Milliseconds -> "ms"
    | Seconds      -> "s"
    | Minutes      -> "min"
    | Hours        -> "h"
    | Days         -> "days"

type Units =
  /// e.g. 'requests' or 'users'; you can put an arbitrary unit here
  | Unit of string
  | Time of TimeUnit
  | Bytes
  | KiB
  | MiB
with
  override x.ToString () =
    match x with
    | Unit u -> u
    | Time t -> t.ToString()
    | Bytes  -> "bytes"
    | KiB    -> "KiB"
    | MiB    -> "MiB"

/// A data point is the name (atom) of a measure taken by a metric. It's not
/// globally unique, but specific to a metric instance.
type DP = DP of PointName
with
  /// Gets the data point as a single string, where each segment is joined by a
  /// dot '.'.
  member x.joined =
    let (DP segs) = x
    String.concat "." segs

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module DP =
  /// Gets the data point as a single string, where each segment is joined by a
  /// dot '.'.
  let joined (DP _ as dp) = dp.joined

/// A measure value is either a float of a int64 value
type MeasureValue =
  | F of float
  | L of int64

/// This is a measured event as it occurred or was at a point in time.
type Measure =
  { /// The value of the measurement
    m_value     : MeasureValue
    /// The identifier for the measure - defaults to the path of the logger
    /// sending it -- this is also the 'name' of the measeure.
    m_path      : DP
    /// When the measurement was taken (start of capture of measure)
    m_timestamp : Instant
    /// The level of the measure
    m_level     : LogLevel
    /// What unit this measure has
    m_unit      : Units
    /// A map of string to anything; it is up to the target in question how to
    /// handle extra data on the metric
    m_data      : Map<string, obj>
    /// A set of tags
    m_tags      : string list }

// TODO: secondary order event
/// This is a calculated event with input of one or many measures and/or global
/// time. f :: Measure list -> Instant -> SecondaryMeasure

/// Module for dealing with measures
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Measure =
  open FSharp.Actor

  open NodaTime

  open Logary
  open Logary.Internals

  /// lenses for the `measure``
  module Lenses =
    open Lenses

    /// value of measurement
    let value_ =
      { get = fun x -> x.m_value
        set = fun v x -> { x with m_value = v } }

    let path_ =
      { get = fun x -> x.m_path
        set = fun v x -> { x with m_path = v } }

    let timestamp_ =
      { get = fun x -> x.m_timestamp
        set = fun v x -> { x with m_timestamp = v } }

    let level_ =
      { get = fun x -> x.m_level
        set = fun v x -> { x with m_level = v } }

    let unit_ =
      { get = fun x -> x.m_unit
        set = fun v x -> { x with m_unit = v } }

    let data_ =
      { get = fun x -> x.m_data
        set = fun v x -> { x with m_data = v } }

    let tags_ =
      { get = fun x -> x.m_tags
        set = fun v x -> { x with m_tags = v } }

    // extras/patterns:

    /// Gets (KeyNotFoundException if not found) or puts the key (idempotently)
    /// to the data Map
    let dataItem_ k =
      { get = fun x -> x.m_data |> Map.find k
        set = fun v x -> { x with m_data = x.m_data |> Map.put k v } }

  ////////////////////
  // Setter methods //
  ////////////////////

  /// Add a key-value pair to the data in the measure
  [<CompiledName "SetData">]
  let setData k = (Lenses.dataItem_ k).set

  /// Add the key-value pairs to the data in the measure
  [<CompiledName "SetDatas">]
  let setDatas datas m = datas |> Seq.fold (fun s (k, v) -> s |> setData k v) m

  /// Sets the path of the measure
  [<CompiledName "SetPath">]
  let setPath = Lenses.path_.set

  /// Sets the timestamp of the measure
  [<CompiledName "SetTimestamp">]
  let setTimestamp value m = { m with m_timestamp = value }

  /// Sets the level of the measure
  [<CompiledName "SetLevel">]
  let setLevel value m = { m with m_level = value }

  /// Sets the unit of the measure
  [<CompiledName "SetUnit">]
  let setUnit value m = { m with m_unit = value }

  [<CompiledName "SetFloat">]
  let setFloat value m = { m with m_value = F value }

  ///////////////////////
  // Factory functions //
  ///////////////////////

  /// An empty `measure`
  let empty =
    { m_value     = F 0.
      m_path      = DP []
      m_timestamp = Date.now()
      m_level     = Info
      m_unit      = Unit "unit"
      m_tags      = []
      m_data      = Map.empty }

  /// Make a new measure value from the type of measure it is, the value and the
  /// path it is taken at or represents
  [<CompiledName "Create">]
  let create path value =
    { empty with m_path  = path
                 m_value = F value }

  /// Make a new measure value from the type of measure it is, the value and the
  /// path it is taken at or represents. This overload splits on dot '.'
  [<CompiledName "Create">]
  let create' (strPath : string) value =
    let p = strPath.Split('.') |> List.ofArray
    { empty with m_path  = DP p
                 m_value = F value }

  /// Make a new measure value from the type of measure it is, the value and the
  /// path it is taken at or represents
  [<CompiledName "FromFloat">]
  let fromFloat path unitt value =
    { empty with m_path  = path
                 m_unit  = unitt
                 m_value = F value }

  [<CompiledName "FromInt64">]
  let fromInt64 path unitt value =
    { empty with m_path  = path
                 m_unit  = unitt
                 m_value = L value }

  let private (<|>) a b : 'a option =
    match a with
    | Some _ -> a
    | None -> b

  open System
  open System.Globalization

  let getValueStr m =
    match m.m_value with
    | F f -> f.ToString(CultureInfo.InvariantCulture)
    | L l -> l.ToString(CultureInfo.InvariantCulture)

  let getValueFloat m =
    match m.m_value with
    | F f -> f
    | L l -> float l

  module LogLine =
    let fromMeasure m =
      LogLine.create
        (getValueStr m) m.m_data m.m_level m.m_tags
        (sprintf "%s[%O]"m.m_path.joined m.m_unit)
        None

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module TimeUnit =
  let ticksPerUnit = function
    | Ticks -> 1.
    | Nanoseconds -> float NodaConstants.TicksPerMillisecond / 1000000.
    /// Ticks as defined by `NodaTime.NodaConstants.TicksPerSecond`.
    | Microseconds -> float NodaConstants.TicksPerMillisecond / 1000.
    | Milliseconds -> float NodaConstants.TicksPerMillisecond
    | Seconds -> float NodaConstants.TicksPerSecond
    | Minutes -> float NodaConstants.TicksPerMinute
    | Hours -> float NodaConstants.TicksPerHour
    | Days -> float NodaConstants.TicksPerStandardDay

namespace NodaTime

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Duration =
  let hours (dur : Duration) =
    float dur.Ticks / float NodaConstants.TicksPerHour

  let minutes (dur : Duration) =
    float dur.Ticks / float NodaConstants.TicksPerMinute

  let seconds (dur : Duration) =
    float dur.Ticks / float NodaConstants.TicksPerSecond

  let milliseconds (dur : Duration) =
    float dur.Ticks / float NodaConstants.TicksPerMillisecond

  let microseconds =
    ((*) 1000.) << milliseconds

  let ticks (dur : Duration) =
    float dur.Ticks

  let nanoseconds =
    ((*) 1000.) << microseconds
