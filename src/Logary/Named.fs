namespace Logary

/// Represents something that is named for the consumer, e.g. a timer,
/// or a logger or a meter.
type Named =
  /// Gets the name of the instance
  abstract name : PointName
