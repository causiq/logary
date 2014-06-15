namespace Logary

/// Main interface used to log LogLines and Metrics.
type Logger =
  inherit Named

  /// Write a log lone to the logger.
  abstract Log    : LogLine -> unit

  /// Write a measure to the logger.
  abstract Metric : Measure -> unit

  /// Gets the currently set log level, aka. the granularity with which things
  /// are being logged
  abstract Level  : LogLevel
