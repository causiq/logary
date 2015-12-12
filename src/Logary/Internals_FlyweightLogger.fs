namespace Logary.Internals

open Logary
open Hopac

/// A logger interface that can be updated in a mutable fashion. Useful
/// for dealing with statics.
type internal FlyweightLogger =
  inherit Logger
  /// Call when the logging framework was configured to set the correct impl
  /// of the logger
  abstract configured : LogaryInstance -> Job<unit>