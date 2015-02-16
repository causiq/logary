namespace log4net.Appender

open System

open NodaTime

open log4net
open log4net.Core
open log4net.Util

open Logary

module internal Impl =

  let memoize<'TIn, 'TOut> pDur (f : 'TIn -> 'TOut) : ('TIn -> 'TOut) =
    let locker = obj()
    let indefinately = TimeSpan.MaxValue = pDur
    let called = ref DateTime.MinValue
    let value = ref Unchecked.defaultof<'TOut>
    let hasValue = ref false
    fun input ->
      // avoiding race-conditions.
      lock locker <| fun _ ->
        if not !hasValue then
          called := DateTime.UtcNow
        if (not !hasValue) || (not indefinately && (!called).Add pDur < DateTime.UtcNow) then
          hasValue := true
          value := f input
          called := DateTime.UtcNow
        !value

/// The functions of the log4net domain and logary codomain.
module Helpers =

  /// Map the log4net.Core.Level to a Logary.LogLevel.
  let mapLogLevel level =
    match level with
    | l when l = Level.Alert         -> LogLevel.Fatal
    | l when l = Level.Emergency     -> LogLevel.Fatal
    | l when l = Level.Critical      -> LogLevel.Fatal
    | l when l = Level.Fatal         -> LogLevel.Fatal
    | l when l = Level.Off           -> LogLevel.Fatal
    | l when l = Level.Severe        -> LogLevel.Fatal
    | l when l = Level.Error         -> LogLevel.Error
    | l when l = Level.Warn          -> LogLevel.Warn
    | l when l = Level.Info          -> LogLevel.Info
    | l when l = Level.Notice        -> LogLevel.Info
    | l when l = Level.Fine          -> LogLevel.Info
    | l when l = Level.Debug         -> LogLevel.Debug
    | l when l = Level.Log4Net_Debug -> LogLevel.Debug
    | l when l = Level.Finer         -> LogLevel.Debug
    | l when l = Level.Finest        -> LogLevel.Verbose
    | l when l = Level.Verbose       -> LogLevel.Verbose
    | l when l = Level.Trace         -> LogLevel.Verbose
    | l when l = Level.All           -> LogLevel.Verbose
    | _ -> LogLevel.Info

  let addProperties (pd : PropertiesDictionary) state =
    pd
    |> Seq.cast<System.Collections.DictionaryEntry>
    |> Seq.map (fun de -> string de.Key, de.Value)
    |> Seq.fold (fun m (key, value) -> m |> Map.add key value) state


type LogaryLog4NetAppender() =
  inherit AppenderSkeleton()

  /// cache loggers for 2000 ms to avoid the loop that looks up appropriate loggers
  let loggerFor = Impl.memoize (TimeSpan.FromMilliseconds 2000.) Logging.getLoggerByName

  override x.Append (evt : LoggingEvent) =
    let msg = base.RenderLoggingEvent(evt)
    let ex = match evt.ExceptionObject with null -> None | e -> Some e
    let data =
      [ "app_domain",       box evt.Domain
        "thread_principal", box evt.Identity
        "code_location",    box evt.LocationInformation
        "thread_name",      box evt.ThreadName
        "user",             box evt.UserName ]
      |> Map.ofList

    let data' = data |> Helpers.addProperties evt.Properties

    LogLine.create msg data' (Helpers.mapLogLevel evt.Level) [] evt.LoggerName ex
    |> LogLine.setTimestamp (Instant.FromDateTimeUtc (evt.TimeStamp.ToUniversalTime()))
    |> Logger.log (loggerFor evt.LoggerName)

  override x.RequiresLayout = true

[<Obsolete("Use LogaryLog4NetAppender")>]
type LogaryAppender() =
  inherit LogaryLog4NetAppender()