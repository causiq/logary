namespace Logary

open FSharp.Actor
open NodaTime

type timeunit =
  | Nanoseconds
  /// Ticks as defined by `NodaTime.NodaConstants.TicksPerSecond`.
  | Ticks
  | Microseconds
  | Milliseconds
  | Seconds
  | Minutes
  | Hours
  | Days

type units =
  /// e.g. 'requests' or 'users'; you can put an arbitrary unit here
  | Unit of string
  | Time of timeunit
  | Bytes
  | KiB
  | MiB

/// This is a measured event as it occurred or was at a point in time.
type ``measure`` =
  { /// The value of the measurement for floats
    m_value     : float option
    /// The value of the measurement for int64s
    m_value'    : int64 option
    /// The value of the measurement for bigints
    m_value''   : bigint option
    /// The identifier for the measure - defaults to the path of the logger
    /// sending it -- this is also the 'name' of the measeure.
    m_path      : string
    /// When the measurement was taken (start of capture of measure)
    m_timestamp : Instant
    /// The level of the measure
    m_level     : LogLevel
    /// What unit this measure has
    m_unit      : units
    /// A map of string to anything; it is up to the target in question how to
    /// handle extra data on the metric
    m_data      : Map<string, obj>
    /// A set of tags
    m_tags      : string list }

// TODO: secondary order event
/// This is a calculated event with input of one or many measures and/or global
/// time. f :: Measure list -> Instant -> SecondaryMeasure

/// Module for dealing with measures
module Measure =
  open FSharp.Actor

  open NodaTime

  open Logary.Internals

  /// lenses for the `measure``
  module Lenses =
    open Lenses

    /// float value
    let value_ =
      { get = fun x -> x.m_value
        set = fun v x -> { x with m_value = v } }

    /// int64 value
    let value'_ =
      { get = fun x -> x.m_value'
        set = fun v x -> { x with m_value' = v } }

    /// bigint value
    let value''_ =
      { get = fun x -> x.m_value''
        set = fun v x -> { x with m_value'' = v } }

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
  let setFloat value m =
    { m with
        m_value = Some value
        m_value' = None
        m_value'' = None }

  [<CompiledName "SetInt64">]
  let setInt64 value m =
    { m with
        m_value   = None
        m_value'  = Some value
        m_value'' = None }

  [<CompiledName "SetBigint">]
  let setBigint value m =
    { m with
        m_value   = None
        m_value'  = None
        m_value'' = Some value }

  ///////////////////////
  // Factory functions //
  ///////////////////////

  /// An empty `measure`
  let empty =
    { m_value     = None
      m_value'    = None
      m_value''   = None
      m_path      = ""
      m_timestamp = Date.now()
      m_level     = Info
      m_unit      = Unit "unit"
      m_tags      = []
      m_data      = Map.empty }

  /// Make a new measure value from the type of measure it is, the value and the
  /// path it is taken at or represents
  [<CompiledName "MkMeasure">]
  let mkMeasure path value =
    { empty with m_path = path
                 m_value = Some value }

  let private (<|>) a b : 'a option =
    match a with
    | Some _ -> a
    | None -> b

  open System
  open System.Globalization

  let private toString x =
    String.Format(CultureInfo.InvariantCulture, "{0}", [| x |])

  let getValueStr m =
    Option.map (box >> toString) m.m_value
    <|> Option.map (box >> toString) m.m_value'
    <|> Option.map (box >> toString) m.m_value''
    |> Option.get

  let getValueFloat m =
    m.m_value
    <|> Option.map float m.m_value'
    <|> Option.map float m.m_value''
    |> Option.get

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
