namespace Logary

/// A counter is just a gauge for an AtomicLong instance. You can increment or
/// decrement its value. For example, we may want a more efficient way of
/// measuring the pending job in a queue:
type Counter =
  inherit Named
  abstract Inc : uint32 -> unit
  abstract Dec : uint32 -> unit

/// A gauge is an instantaneous measurement of a value.
/// This interface is implemented by the user and registered in Logary.
type Gauge =
  inherit Named
  abstract Put : float -> unit

/// Module for working with metrics
module Metrics =
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

  /////////////////////
  // Logging methods //
  /////////////////////

  /// Make a new measure value from the type of measure it is, the value and the
  /// path it is taken at or represents
  [<CompiledName "MkMeasure">]
  let mkMeasure path value =
    { m_value     = value
      m_path      = path
      m_timestamp = Date.utcNow()
      m_level     = Info
      m_unit      = Units.Unit "unit"
      m_tags      = []
      m_data      = Map.empty }

  /// Write a metric/measure of your chosing
  [<CompiledName "Metric">]
  let metric logger ms =
    (logger : Logger).Metric ms

  /// Increment the counter at the path 'path' at the info level
  let incr logger path = mkMeasure path 1. |> metric logger

  /// Increment the counter at the path 'path' at the info level by the amount
  let incrBy logger path amount = mkMeasure path amount |> metric logger

  /// Decrement the counter at the path 'path' at the info level
  let decr logger path = mkMeasure path -1. |> metric logger

  /// Decrement the counter at the path 'path' at the info level by the amount
  let decrBy logger path amount = mkMeasure path -amount |> metric logger

  /// Give a gauge value at this current instant in time
  let gauge logger path amount = mkMeasure path amount |> metric logger
