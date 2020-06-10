namespace Logary.Trace

open Logary

/// Useful for interacting with Logary's internal data object model.
type SpanOpsAdvanced =
  /// Call to stop the timer/Span. This method call is non-blocking, but executes the callback on the caller's thread.
  abstract finish: transform: (Model.SpanMessage -> unit) -> SpanMessage

type SpanLogger =
  inherit LoggerScope
  inherit Span
  inherit SpanOpsAdvanced

  /// Call to stop the timer/Span. The `transform` callback is not run until the LogResult is selected/awaited/run,
  /// and unless the `LogResult` is used, the `Message` will not be logged. This function is used internally to allow
  /// testing and ensuring the logging pipeline, end to end.
  abstract finishWithAck: transform:(Model.SpanMessage -> unit) -> LogResult

  abstract finishWithAck: ts:EpochNanoSeconds -> LogResult
  abstract finishWithAck: unit -> LogResult

  /// Makes the `SpanLogger` log through to its Logger as well as keeping the events
  /// in the Span.
  abstract logThrough: unit -> unit
