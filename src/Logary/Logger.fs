namespace Logary

open System
open Hopac
open NodaTime
open Logary

/// Patterns to match against the context; useful for extracting the data
/// slightly more semantically than "obj"-everything. Based on the known prefixes
/// in `KnownLiterals`.
type internal ProcessResult = Result<Promise<unit>, ControlMessage>
type internal LogResult = Alt<ProcessResult>

/// See the docs on the functions for descriptions on how Ack works in conjunction
/// with the Promise.
type Logger =
  /// The PointName for this `Logger`: corresponds to the `name` field for the
  /// `Messages` produced from this instance.
  ///
  /// To use a car metaphor, this could be Fleet.Cars.Volvo.X90 and you could then
  /// add gauges for each sub-measurement/sensor; like Engine.RPM or Engine.Temperature.
  ///
  /// This field is NOT the same as the Span operation/label/name, instead see `Span.label` for that.
  abstract name: PointName

  /// Returns an Alt that commits on ALL N Targets' buffers accepting the message.
  /// Even if the Alt was not committed to, one or more targets (fewer than N) may have accepted the message.
  /// And this can not be canceled.
  ///
  /// - `waitForAccept`:
  ///     `true` means waiting for each target buffer to be available to take the message.
  ///     `false` means it will try to detect whether the buffer is full and return immediately.
  abstract logWithAck: waitForAccept: bool * message: LogaryMessage -> LogResult

  /// Gets the currently set log level (minimal,inclusive),
  /// aka. the granularity with which things are being logged.
  abstract level: LogLevel

/// A disposable interface to use with `use` constructs and to create child-
/// contexts. Since it inherits Logger, you can pass this scope down into child
/// function calls. This interface should dovetail with how Zipkin/Dapper
/// manages parent/child spans.
type LoggerScope =
  inherit IDisposable
  inherit Logger
