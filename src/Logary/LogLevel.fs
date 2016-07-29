namespace Logary

open System
open System.ComponentModel
open NodaTime
open Logary.Utils.Chiron
open Logary.Utils.Chiron.Operators

/// The log levels specify the severity of the message.
[<CustomEquality; CustomComparison>]
type LogLevel =
  /// The log message is not that important; can be used for intricate debugging.
  | Verbose
  /// The log message is at a default level, debug level. Useful for shipping to
  /// infrastructure that further processes it, but not so useful for human
  /// inspection in its raw format, except during development.
  | Debug
  /// The log message is informational; e.g. the service started, stopped or
  /// some important business event occurred.
  | Info
  /// The log message is a warning; e.g. there was an unhandled exception or
  /// an even occurred which was unexpected. Sometimes human corrective action
  /// is needed.
  | Warn
  /// The log message is at an error level, meaning an unhandled exception
  /// occurred at a location where it is deemed important to keeping the service
  /// running. A human should take corrective action.
  | Error
  /// The log message denotes a fatal error which cannot be recovered from. The
  /// service should be shut down. Human corrective action is needed.
  | Fatal
with
  /// Converts the LogLevel to a string
  override x.ToString () =
    match x with
    | Verbose -> "verbose"
    | Debug   -> "debug"
    | Info    -> "info"
    | Warn    -> "warn"
    | Error   -> "error"
    | Fatal   -> "fatal"

  /// Converts the string passed to a Loglevel.
  [<EditorBrowsable(System.ComponentModel.EditorBrowsableState.Never)>]
  static member ofString str =
    if str = null then invalidArg "str" "may not be null"
    match String.toLowerInvariant str with
    | "verbose" -> Verbose
    | "debug"   -> Debug
    | "info"    -> Info
    | "warn"    -> Warn
    | "error"   -> Error
    | "fatal"   -> Fatal
    | _         -> Info

  static member FromJson (_ : LogLevel) =
    LogLevel.ofString <!> Json.Lens.getPartial Json.String_

  static member ToJson (l : LogLevel) : Json<unit> =
    Json.Lens.setPartial Json.String_ (l.ToString())

  /// Turn the LogLevel into an integer
  [<EditorBrowsable(EditorBrowsableState.Never)>]
  member x.toInt () =
    (function
    | Verbose -> 1
    | Debug   -> 2
    | Info    -> 3
    | Warn    -> 4
    | Error   -> 5
    | Fatal   -> 6) x

  /// Turn an integer into a LogLevel
  [<EditorBrowsable(EditorBrowsableState.Never)>]
  static member ofInt i =
    (function
    | 1 -> Verbose
    | 2 -> Debug
    | 3 -> Info
    | 4 -> Warn
    | 5 -> Error
    | 6 -> Fatal
    | _ as i -> failwithf "LogLevel matching integer %i is not available" i) i

  interface IComparable<LogLevel> with
    member x.CompareTo other =
      compare (x.toInt()) (other.toInt())

  static member op_LessThan (a, b) =
    (a :> IComparable<LogLevel>).CompareTo(b) < 0

  static member op_LessThanOrEqual (a, b) =
    (a :> IComparable<LogLevel>).CompareTo(b) <= 0

  static member op_GreaterThan (a, b) =
    (a :> IComparable<LogLevel>).CompareTo(b) > 0

  static member op_GreaterThanOrEqual (a, b) =
    (a :> IComparable<LogLevel>).CompareTo(b) >= 0

  override x.GetHashCode () =
    x.toInt ()

  interface IComparable with
    member x.CompareTo other =
      match other with
      | null ->
        1

      | :? LogLevel as tother ->
        (x :> IComparable<LogLevel>).CompareTo tother

      | _ ->
        failwithf "invalid comparison %A to %A" x other

  interface IEquatable<LogLevel> with
    member x.Equals other =
      x.toInt() = other.toInt()
  
  override x.Equals other =
    (x :> IComparable).CompareTo other = 0

  // http://stackoverflow.com/questions/5557899/f-implement-icomparable-for-hashseta
