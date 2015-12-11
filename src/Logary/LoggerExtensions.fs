namespace Logary

open System
open System.Runtime.CompilerServices
open NodaTime
open Logary
open Logary.Utils.Aether
open Logary.Internals

/// Extensions for logging to a Logger
[<Extension>]
module LoggerExtensions =

  // TODO/CONSIDER?

  /// Log a message to the log
  [<Extension; CompiledName("Log")>]
  let log (logger : Logger, template, level, name : PointName, timestamp : Nullable<Instant>) =
    if template = null then nullArg "template"

    let setTimestamp =
      if timestamp.HasValue
      then Message.setTicks timestamp.Value.Ticks
      else Message.updateTimestamp

    Message.event level template
    |> setTimestamp
    |> Message.setName name
    |> logger.log

  /// Log a message to the log
  [<Extension; CompiledName("Log")>]
  let logAnnotate (logger : Logger, template, level) =
    if template = null then nullArg "template"
    Message.event level template
    |> logger.log
