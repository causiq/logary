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
module LogaryHelpers =

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

open LogaryHelpers

type LogaryLog4NetAppender() =
  inherit AppenderSkeleton()

  /// cache loggers for 2000 ms to avoid the loop that looks up appropriate loggers
  let loggerFor =
    Impl.memoize (TimeSpan.FromMilliseconds 2000.)
                 Logging.getLoggerByName

  let formatCodeInfo (codeLocation:LocationInfo) =
    let sb = Text.StringBuilder();

    codeLocation.StackFrames
    |> Array.iteri (fun index frame ->
         if index = 0 then
            sb.AppendLine(frame.FullInfo) |> ignore
         else
           sb.AppendFormat("\tfrom {0}{1}",frame.FullInfo,Environment.NewLine) |> ignore
    )

    sb.ToString()

  override x.Append (evt : LoggingEvent) =
    let msg = base.RenderLoggingEvent(evt)
    let ex = match evt.ExceptionObject with null -> None | e -> Some e

    let codeLocation = formatCodeInfo evt.LocationInformation

    let data =
      [ "app_domain",       box evt.Domain
        "thread_principal", box evt.Identity
        "code_location",    box codeLocation
        "thread_name",      box evt.ThreadName
        "user",             box evt.UserName ]
      |> Map.ofList

    let data' = data |> LogaryHelpers.addProperties evt.Properties

    Message.event (LogaryHelpers.mapLogLevel evt.Level) msg
    |> Message.setFieldsFromMap data'
    |> Message.setName (PointName.parse evt.LoggerName)
    |> (ex |> Option.fold (fun s -> Message.addExn) id)
    |> Message.setUTCTicks (evt.TimeStamp.ToUniversalTime()).Ticks
    |> Logger.logSimple (loggerFor evt.LoggerName)

  override x.RequiresLayout = true

[<Obsolete("Use LogaryLog4NetAppender")>]
type LogaryAppender() =
  inherit LogaryLog4NetAppender()