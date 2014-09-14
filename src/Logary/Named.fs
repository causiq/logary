namespace Logary

open System

/// Represents something that is named for the consumer, e.g. a timer,
/// or a logger or a meter.
type Named =
  inherit IComparable<string>
  inherit IEquatable<string>
  /// Gets the name of the instance
  abstract Name : string
