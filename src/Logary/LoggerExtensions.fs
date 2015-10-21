namespace Logary

open System
open System.Runtime.CompilerServices

open Logary.Utils.Aether
open Logary.Internals
open Logary.DataModel

/// Extensions for logging to a Logger
[<Extension>]
module LoggerExtensions =

  // TODO/CONSIDER?

  /// Log a message to the log
  [<Extension; CompiledName("Log")>]
  let log (logger : Logger, message, level, service, timestamp : Nullable<NodaTime.Instant>) =
    if message = null then nullArg "message"
    let msg = Message.event level message
    Message.setTimestamp (if timestamp.HasValue then timestamp.Value.Ticks else msg.timestamp) msg
    |> Message.Context.serviceSet service
    |> logger.Log

  /// Log a message to the log
  [<Extension; CompiledName("Log")>]
  let logAnnotate (logger : Logger, message, level) =
    if message = null then nullArg "message"
    Message.event level message
    |> logger.Log
