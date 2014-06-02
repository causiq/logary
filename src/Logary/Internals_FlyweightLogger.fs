namespace Logary.Internals

open Logary

/// A logger interface that can be updated in a mutable fashion. Useful
/// for dealing with statics.
type internal FlyweightLogger =
  inherit Logger
  /// Call when the logging framework was configured to set the correct impl
  /// of the logger
  abstract Configured : LogManager -> unit
