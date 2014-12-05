namespace Logary.Topshelf

open System

open Topshelf.Logging

open System.Diagnostics

open Logary

type LogaryLogWriter(logger : Logger) =

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

  let setLevel (level : LoggingLevel) (l : LogLine) =
    { l with level = fromLoggingLevel level }

  let objToLine : obj -> LogLine = function
    | :? string as s -> { LogLine.empty with message = s }
    | o -> LogLine.create "unknown data" Map.empty Info [] logger.Name None

  let log = LogLine.setPath logger.Name >> Logger.log logger

  interface LogWriter with

    member x.IsDebugEnabled = logger.Level <= Debug
    member x.IsInfoEnabled = logger.Level <= Info
    member x.IsWarnEnabled = logger.Level <= Warn
    member x.IsErrorEnabled = logger.Level <= Error
    member x.IsFatalEnabled = logger.Level <= Fatal

    /////////////////// Log ///////////////
    member x.Log (level, o : obj) =
      o |> objToLine |> setLevel level |> log

    member x.Log (level, o : obj, e : exn) =
      o |> objToLine |> setLevel level |> LogLine.setExn e |> log

    member x.Log (level, fO : LogWriterOutputProvider) =
      if logger.Level <= fromLoggingLevel level then
        (fO.Invoke >> objToLine >> setLevel level) () |> log

    member x.LogFormat (level, formatProvider, format, args) =
      { LogLine.empty with
          message = String.Format(formatProvider, format, args)
          level   = fromLoggingLevel level }
      |> log

    member x.LogFormat (level, format, args) =
      { LogLine.empty with
          message = String.Format(format, args)
          level   = fromLoggingLevel level }
      |> log

    /////////////////// DEBUG ///////////////
    member x.Debug (o : obj) =
      o |> objToLine |> LogLine.setLevel Debug |> log

    member x.Debug (o, ex) =
      o |> objToLine |> LogLine.setLevel Debug |> LogLine.setExn ex |> log

    member x.Debug (fO : LogWriterOutputProvider) =
      (fO.Invoke >> objToLine >> LogLine.setLevel Debug) () |> log

    member x.DebugFormat (formatProvider, format, args) =
      LogLine.create' Debug (String.Format(formatProvider, format, args)) |> log

    member x.DebugFormat (format, args) =
      LogLine.create' Debug (String.Format(format, args)) |> log

    /////////////////// INFO ///////////////
    member x.Info (o : obj) =
      o |> objToLine |> LogLine.setLevel Info |> log

    member x.Info (o, ex) =
      o |> objToLine |> LogLine.setLevel Info |> LogLine.setExn ex |> log

    member x.Info (fO : LogWriterOutputProvider) =
      (fO.Invoke >> objToLine >> LogLine.setLevel Info) () |> log

    member x.InfoFormat (formatProvider, format, args) =
      LogLine.create' Info (String.Format(formatProvider, format, args)) |> log

    member x.InfoFormat (format, args) =
      LogLine.create' Info (String.Format(format, args)) |> log

    /////////////////// WARN ///////////////
    member x.Warn (o : obj) =
      o |> objToLine |> LogLine.setLevel Warn |> log

    member x.Warn (o, ex) =
      o |> objToLine |> LogLine.setLevel Warn |> LogLine.setExn ex |> log

    member x.Warn (fO : LogWriterOutputProvider) =
      (fO.Invoke >> objToLine >> LogLine.setLevel Warn) () |> log

    member x.WarnFormat (formatProvider, format, args) =
      LogLine.create' Warn (String.Format(formatProvider, format, args)) |> log

    member x.WarnFormat (format, args) =
      LogLine.create' Warn (String.Format(format, args)) |> log

    /////////////////// ERROR ///////////////
    member x.Error (o : obj) =
      o |> objToLine |> LogLine.setLevel Error |> log

    member x.Error (o, ex) =
      o |> objToLine |> LogLine.setLevel Error |> LogLine.setExn ex |> log

    member x.Error (fO : LogWriterOutputProvider) =
      (fO.Invoke >> objToLine >> LogLine.setLevel Error) () |> log

    member x.ErrorFormat (formatProvider, format, args) =
      LogLine.create' Error (String.Format(formatProvider, format, args)) |> log

    member x.ErrorFormat (format, args) =
      LogLine.create' Error (String.Format(format, args)) |> log

    /////////////////// FATAL ///////////////
    member x.Fatal (o : obj) =
      o |> objToLine |> LogLine.setLevel Fatal |> log

    member x.Fatal (o, ex) =
      o |> objToLine |> LogLine.setLevel Fatal |> LogLine.setExn ex |> log

    member x.Fatal (fO : LogWriterOutputProvider) =
      (fO.Invoke >> objToLine >> LogLine.setLevel Fatal) () |> log

    member x.FatalFormat (formatProvider, format, args) =
      LogLine.create' Fatal (String.Format(formatProvider, format, args)) |> log

    member x.FatalFormat (format, args) =
      LogLine.create' Fatal (String.Format(format, args)) |> log
