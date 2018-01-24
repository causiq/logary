namespace CommonLogging

open System
open System.Globalization
open System.Collections.Generic

open Common.Logging

open Logary

type internal Adapter(logger : Logger) =

  let stubVariablesContext () =
    let dic = new Dictionary<string, obj>()
    { new IVariablesContext with
        member x.Set (key, value) =
          dic.[key] <- value
        member x.Get key = dic.[key]
        member x.Contains key = dic.ContainsKey key
        member x.Remove key = dic.Remove key |> ignore
        member x.Clear () = dic.Clear () }

  let gc = stubVariablesContext ()
  let tc = stubVariablesContext ()

  let objToLine : obj -> Message = function
    | :? string as s ->
      Message.event Debug s

    | o ->
      Message.event Info "Unknown Data from log4net"
      |> Message.setContext "data" o
      |> Message.setName logger.name

  let log =
    Message.setName logger.name >> Logger.logSimple logger

  let llog level =
    Message.setLevel level >> log

  let invariantCulture = CultureInfo.InvariantCulture

  let fmt formatProvider format args = String.Format(formatProvider, format, args)

  let write (message : obj) level =
    message |> objToLine |> llog level

  let write' (message : obj) level ex =
    message
    |> objToLine
    |> Message.addExn ex
    |> llog level

  let write'' formatProvider format level ex args =
    fmt formatProvider format args
    |> Message.event level
    |> (ex |> Option.fold (fun s -> Message.addExn) id)
    |> log

  let write''' (formatProvider : IFormatProvider)
               (cb : Action<FormatMessageHandler>)
               (ex : exn option)
               level =
    cb.Invoke(
      FormatMessageHandler(
        fun format args ->
          let res = fmt formatProvider format args
          res
          |> Message.event level
          |> (ex |> Option.fold (fun s -> Message.addExn) id)
          |> log
          res))

  // fucking too big interface
  interface ILog with

    member x.IsTraceEnabled = logger.level <= Verbose
    member x.IsDebugEnabled = logger.level <= Debug
    member x.IsInfoEnabled = logger.level <= Info
    member x.IsWarnEnabled = logger.level <= Warn
    member x.IsErrorEnabled = logger.level <= Error
    member x.IsFatalEnabled = logger.level <= Fatal

    member x.Trace (message : obj) = write message Verbose
    member x.Trace (message : obj, ex) = write' message Verbose ex
    member x.TraceFormat (format, args) = write'' invariantCulture format Verbose None args
    member x.TraceFormat (formatProvider, format, args) = write'' formatProvider format Verbose None args
    member x.TraceFormat (format, ex, args) = write'' invariantCulture format Verbose (Some ex) args
    member x.TraceFormat (formatProvider, format, ex, args) =
      write'' formatProvider format Verbose (Some ex) args
    member x.Trace cb = write''' invariantCulture cb None Verbose
    member x.Trace (cb, ex) = write''' invariantCulture cb (Some ex) Verbose
    member x.Trace (formatProvider, cb) = write''' formatProvider cb None Verbose
    member x.Trace (formatProvider, cb, ex) = write''' formatProvider cb (Some ex) Verbose

    member x.Debug (message : obj) = write message Debug
    member x.Debug (message : obj, ex) = write' message Debug ex
    member x.DebugFormat (format, args) = write'' invariantCulture format Debug None args
    member x.DebugFormat (formatProvider, format, args) = write'' formatProvider format Debug None args
    member x.DebugFormat (format, ex, args) = write'' invariantCulture format Debug (Some ex) args
    member x.DebugFormat (formatProvider, format, ex, args) =
      write'' formatProvider format Debug (Some ex) args
    member x.Debug cb = write''' invariantCulture cb None Debug
    member x.Debug (cb, ex) = write''' invariantCulture cb (Some ex) Debug
    member x.Debug (formatProvider, cb) = write''' formatProvider cb None Debug
    member x.Debug (formatProvider, cb, ex) = write''' formatProvider cb (Some ex) Debug

    member x.Info (message : obj) = write message Info
    member x.Info (message : obj, ex) = write' message Info ex
    member x.InfoFormat (format, args) = write'' invariantCulture format Info None args
    member x.InfoFormat (formatProvider, format, args) = write'' formatProvider format Info None args
    member x.InfoFormat (format, ex, args) = write'' invariantCulture format Info (Some ex) args
    member x.InfoFormat (formatProvider, format, ex, args) =
      write'' formatProvider format Info (Some ex) args
    member x.Info cb = write''' invariantCulture cb None Info
    member x.Info (cb, ex) = write''' invariantCulture cb (Some ex) Info
    member x.Info (formatProvider, cb) = write''' formatProvider cb None Info
    member x.Info (formatProvider, cb, ex) = write''' formatProvider cb (Some ex) Info

    member x.Warn (message : obj) = write message Warn
    member x.Warn (message : obj, ex) = write' message Warn ex
    member x.WarnFormat (format, args) = write'' invariantCulture format Warn None args
    member x.WarnFormat (formatProvider, format, args) = write'' formatProvider format Warn None args
    member x.WarnFormat (format, ex, args) = write'' invariantCulture format Warn (Some ex) args
    member x.WarnFormat (formatProvider, format, ex, args) =
      write'' formatProvider format Warn (Some ex) args
    member x.Warn cb = write''' invariantCulture cb None Warn
    member x.Warn (cb, ex) = write''' invariantCulture cb (Some ex) Warn
    member x.Warn (formatProvider, cb) = write''' formatProvider cb None Warn
    member x.Warn (formatProvider, cb, ex) = write''' formatProvider cb (Some ex) Warn

    member x.Error (message : obj) = write message Error
    member x.Error (message : obj, ex) = write' message Error ex
    member x.ErrorFormat (format, args) = write'' invariantCulture format Error None args
    member x.ErrorFormat (formatProvider, format, args) = write'' formatProvider format Error None args
    member x.ErrorFormat (format, ex, args) = write'' invariantCulture format Error (Some ex) args
    member x.ErrorFormat (formatProvider, format, ex, args) =
      write'' formatProvider format Error (Some ex) args
    member x.Error cb = write''' invariantCulture cb None Error
    member x.Error (cb, ex) = write''' invariantCulture cb (Some ex) Error
    member x.Error (formatProvider, cb) = write''' formatProvider cb None Error
    member x.Error (formatProvider, cb, ex) = write''' formatProvider cb (Some ex) Error

    member x.Fatal (message : obj) = write message Fatal
    member x.Fatal (message : obj, ex) = write' message Fatal ex
    member x.FatalFormat (format, args) = write'' invariantCulture format Fatal None args
    member x.FatalFormat (formatProvider, format, args) = write'' formatProvider format Fatal None args
    member x.FatalFormat (format, ex, args) = write'' invariantCulture format Fatal (Some ex) args
    member x.FatalFormat (formatProvider, format, ex, args) =
      write'' formatProvider format Fatal (Some ex) args
    member x.Fatal cb = write''' invariantCulture cb None Fatal
    member x.Fatal (cb, ex) = write''' invariantCulture cb (Some ex) Fatal
    member x.Fatal (formatProvider, cb) = write''' formatProvider cb None Fatal
    member x.Fatal (formatProvider, cb, ex) = write''' formatProvider cb (Some ex) Fatal

    member x.GlobalVariablesContext = gc
    member x.ThreadVariablesContext = tc

    member x.NestedThreadVariablesContext = raise (NotImplementedException())

type LogaryAdapter(lm : LogManager) =
  interface ILoggerFactoryAdapter with
    member x.GetLogger (typ : Type) =
      Adapter(PointName.parse typ.FullName |> lm.getLogger) :> ILog
    member x.GetLogger (name : string) =
      Adapter(PointName.parse name |> lm.getLogger) :> ILog