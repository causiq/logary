module Logary.Targets.NLog

type private Marker = interface end

open NLog
open NLog.Config

open FSharp.Actor

open Logary
open Logary.Internals
open Logary.Target
open Logary.Target.TargetUtils

let private toNLogLevel = function
  | Verbose -> NLog.LogLevel.Trace
  | Debug   -> NLog.LogLevel.Debug
  | Info    -> NLog.LogLevel.Info
  | Warn    -> NLog.LogLevel.Warn
  | Error   -> NLog.LogLevel.Error
  | Fatal   -> NLog.LogLevel.Fatal

let private getNLogger name =
  NLog.LogManager.GetLogger name

let private nlogLoop nlogConfig metadata =
  (fun (inbox : IActor<_>) ->
    let rec loop () = async {
      do NLog.LogManager.Configuration <- nlogConfig
      return! running () }
    and running () = async {
      let! msg, mopts = inbox.Receive()
      match msg with
      | Flush chan -> chan.Reply Ack
      | Log l ->
        let logger = getNLogger l.path
        let event : LogEventInfo =
          new LogEventInfo(
            level = (l.level |> toNLogLevel),
            loggerName = l.path,
            formatProvider = System.Globalization.CultureInfo.InvariantCulture,
            message = l.message,
            parameters = [| |],
            ``exception`` = null)
        // http://stackoverflow.com/questions/2297236/how-to-get-type-of-the-module-in-f
        logger.Log(typeof<Marker>.DeclaringType, event)
        return! running ()
      | Measure m -> return! loop ()
      | ShutdownTarget ackChan -> return shutdown () }

    and shutdown () = NLog.LogManager.Shutdown()

    loop ())

let create nlogConfig = stdNamedTarget (nlogLoop nlogConfig)
