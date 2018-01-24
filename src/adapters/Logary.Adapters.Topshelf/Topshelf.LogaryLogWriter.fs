namespace Topshelf.Logging

open System
open System.Runtime.CompilerServices
open System.Diagnostics
open Topshelf.Logging
open Topshelf.HostConfigurators
open Logary

type TopshelfAdapter(logger : Logger) =

  let fromSourceLevel = function
    | SourceLevels.Off
    | SourceLevels.Verbose     -> Verbose
    | SourceLevels.All         -> Debug
    | SourceLevels.Information -> Info
    | SourceLevels.Warning     -> Warn
    | SourceLevels.Critical    -> Error
    | SourceLevels.Error       -> Fatal
    | _                        -> Verbose

  let fromLoggingLevel (level : LoggingLevel) =
    fromSourceLevel level.SourceLevel

  let setLevel (level : LoggingLevel) (l : Message) =
    { l with level = fromLoggingLevel level }

  let objToLine : obj -> Message = function
    | :? string as s ->
      Message.event Debug s

    | o ->
      Message.event Info "unknown data"
      |> Message.setContext "data" o
      |> Message.setName logger.name

  let log =
    Message.setName logger.name
    >> Logger.logSimple logger

  interface LogWriter with

    member x.IsDebugEnabled = logger.level <= Debug
    member x.IsInfoEnabled = logger.level <= Info
    member x.IsWarnEnabled = logger.level <= Warn
    member x.IsErrorEnabled = logger.level <= Error
    member x.IsFatalEnabled = logger.level <= Fatal

    /////////////////// Log ///////////////
    member x.Log (level, o : obj) =
      o |> objToLine |> setLevel level |> log

    member x.Log (level, o : obj, e : exn) =
      o |> objToLine |> setLevel level |> Message.addExn e |> log

    member x.Log (level, fO : LogWriterOutputProvider) =
      (fO.Invoke >> objToLine >> setLevel level) () |> log

    member x.LogFormat (level, formatProvider, format, args) =
      Message.eventFormat (fromLoggingLevel level, format, args)
      |> log

    member x.LogFormat (level, format, args) =
      Message.eventFormat (fromLoggingLevel level, format, args)
      |> log

    /////////////////// DEBUG ///////////////
    member x.Debug (o : obj) =
      o |> objToLine |> Message.setLevel Debug |> log

    member x.Debug (o, ex) =
      o |> objToLine |> Message.setLevel Debug |> Message.addExn ex |> log

    member x.Debug (fO : LogWriterOutputProvider) =
      (fO.Invoke >> objToLine >> Message.setLevel Debug) () |> log

    member x.DebugFormat (formatProvider, format, args) =
      Message.eventFormat (Debug, format, args)
      |> log

    member x.DebugFormat (format, args) =
      Message.eventFormat (Debug, format, args)
      |> log

    /////////////////// INFO ///////////////
    member x.Info (o : obj) =
      o |> objToLine |> Message.setLevel Info |> log

    member x.Info (o, ex) =
      o |> objToLine |> Message.setLevel Info |> Message.addExn ex |> log

    member x.Info (fO : LogWriterOutputProvider) =
      (fO.Invoke >> objToLine >> Message.setLevel Info) () |> log

    member x.InfoFormat (formatProvider, format, args) =
      Message.eventFormat (Info, format, args)
      |> log

    member x.InfoFormat (format, args) =
      Message.eventFormat (Info, format, args)
      |> log

    /////////////////// WARN ///////////////
    member x.Warn (o : obj) =
      o |> objToLine |> Message.setLevel Warn |> log

    member x.Warn (o, ex) =
      o |> objToLine |> Message.setLevel Warn |> Message.addExn ex |> log

    member x.Warn (fO : LogWriterOutputProvider) =
      (fO.Invoke >> objToLine >> Message.setLevel Warn) () |> log

    member x.WarnFormat (formatProvider, format, args) =
      Message.eventFormat (Warn, format, args)
      |> log

    member x.WarnFormat (format, args) =
      Message.eventFormat (Warn, format, args)
      |> log

    /////////////////// ERROR ///////////////
    member x.Error (o : obj) =
      o |> objToLine |> Message.setLevel Error |> log

    member x.Error (o, ex) =
      o |> objToLine |> Message.setLevel Error |> Message.addExn ex |> log

    member x.Error (fO : LogWriterOutputProvider) =
      (fO.Invoke >> objToLine >> Message.setLevel Error) () |> log

    member x.ErrorFormat (formatProvider, format, args) =
      Message.eventFormat (Error, format, args)
      |> log

    member x.ErrorFormat (format, args) =
      Message.eventFormat (Error, format, args)
      |> log

    /////////////////// FATAL ///////////////
    member x.Fatal (o : obj) =
      o |> objToLine |> Message.setLevel Fatal |> log

    member x.Fatal (o, ex) =
      o |> objToLine |> Message.setLevel Fatal |> Message.addExn ex |> log

    member x.Fatal (fO : LogWriterOutputProvider) =
      (fO.Invoke >> objToLine >> Message.setLevel Fatal) () |> log

    member x.FatalFormat (formatProvider, format, args) =
      Message.eventFormat (Fatal, format, args)
      |> log

    member x.FatalFormat (format, args) =
      Message.eventFormat (Error, format, args)
      |> log

type LogaryWriterFactory(logary : LogManager) =
  interface LogWriterFactory with
    member x.Get name =
      let llogger = logary.getLogger (PointName.parse name)
      TopshelfAdapter llogger :> LogWriter
    member x.Shutdown () =
      ()

type LogaryHostLoggerConfigurator(logary : LogManager) =
  interface HostLoggerConfigurator with
    member x.CreateLogWriterFactory() =
      LogaryWriterFactory(logary) :> LogWriterFactory

[<Extension>]
module ConfiguratorExtensions =

  [<Extension; CompiledName "UseLogary">]
  let useLogary (hc : HostConfigurator) (logary : LogManager) =
    HostLogger.UseLogger(new LogaryHostLoggerConfigurator(logary))