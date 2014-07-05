namespace Logary

open FSharp.Actor
open NodaTime

type Units =
  /// e.g. 'requests' or 'users'; you can put an arbitrary unit here
  | Unit of string
  | Bytes
  | KiB
  | MiB
  | Seconds
  | Ticks

/// This is a measured event as it occurred or was at a point in time.
type ``measure`` =
  { /// The value of the measurement
    m_value     : float option

    m_value'    : int64 option

    m_value''   : bigint option
    /// The identifier for the measure - defaults to the path of the logger
    /// sending it -- this is also the 'name' of the measeure.
    m_path      : string
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
type sndmeasure =
  { sm_measures  : ``measure`` list
    sm_data      : ``measure`` }

module Measure =
  open FSharp.Actor

  open NodaTime

  open Logary.Internals

  /// An abstract base class for metric objects with actors being the backing
  /// primitive.
  [<AbstractClass>]
  type MetricInstance(name, targets) =
    member x.Targets : IActor list = targets
    member x.Name = name
    interface Named with
      member x.Name = name

  ////////////////////
  // Getter methods //
  ////////////////////

  let value m = m.m_value
  let path m = m.m_path
  let timestamp m = m.m_timestamp
  let level m = m.m_level
  let data m = m.m_data
  let tags m = m.m_tags

  ////////////////////
  // Setter methods //
  ////////////////////

  /// Add a key-value pair to the data in the measure
  [<CompiledName "SetData">]
  let setData k v m = { m with m_data = m.m_data |> Map.put k v }

  /// Add the key-value pairs to the data in the measure
  [<CompiledName "SetDatas">]
  let setDatas datas m = datas |> Seq.fold (fun s (k, v) -> s |> setData k v) m

  /// Sets the path of the measure
  [<CompiledName "SetPath">]
  let setPath p m = { m with m_path = p }

  /// Sets the value of the measure
  [<CompiledName "SetValue">]
  let setValue value m = { m with m_value = value }

  /// Sets the timestamp of the measure
  [<CompiledName "SetTimestamp">]
  let setTimestamp value m = { m with m_timestamp = value }

  /// Sets the level of the measure
  [<CompiledName "SetLevel">]
  let setLevel value m = { m with m_level = value }

  /// Sets the unit of the measure
  [<CompiledName "SetUnit">]
  let setUnit value m = { m with m_unit = value }

  let setFloat value m =
    { m with
        m_value = Some value
        m_value' = None
        m_value'' = None }

  let setInt64 value m =
    { m with
        m_value   = None
        m_value'  = Some value
        m_value'' = None }

  let setBigInt value m =
    { m with
        m_value   = None
        m_value'  = None
        m_value'' = Some value }

  ///////////////////////
  // Factory functions //
  ///////////////////////

  let empty =
    { m_value     = None
      m_value'    = None
      m_value''   = None
      m_path      = ""
      m_timestamp = Date.utcNow()
      m_level     = Info
      m_unit      = Units.Unit "unit"
      m_tags      = []
      m_data      = Map.empty }

  /// Make a new measure value from the type of measure it is, the value and the
  /// path it is taken at or represents
  [<CompiledName "MkMeasure">]
  let mkMeasure path value =
    { empty with m_path = path
                 m_value = value }
