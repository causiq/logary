namespace Logary

open System
open NodaTime

/// The log levels specify the severity of the message.
[<StructuralEquality; NoComparison>]
type LogLine =
  /// The message for the log line
  { message       : string
  /// A dictionary-alike object that keeps data to be logged
  ; data          : Map<string, obj>
  /// A log level
  ; level         : LogLevel
  /// A list of tags in the log line
  ; tags          : string list
  /// When the log line was created
  ; timestamp     : Instant
  /// Normally a URI or a hierachy: can be filled out by the logging framework.
  /// It denotes the location that the log or metric was sent from.
  ; path          : string
  /// Optional exception
  ; ``exception`` : exn option }
  static member Create(message, ?level, ?path, ?data, ?tags, ?timestap, ?``exception``) =
    { message       = message
    ; data          = defaultArg data Map.empty
    ; level         = defaultArg level LogLevel.Info
    ; tags          = defaultArg tags []
    ; timestamp     = Internals.Date.utcNow ()
    ; path          = defaultArg path ""
    ; ``exception`` = defaultArg ``exception`` None }
