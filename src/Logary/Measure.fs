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
type Measure =
  { /// The value of the measurement
    m_value     : float
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
type SecondaryMeasure =
  { sm_measures  : Measure list
    sm_data      : Measure }