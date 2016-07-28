namespace Logary.Facade

open System
open System.Runtime.CompilerServices

type LogLevel =
  | Verbose
  | Debug
  | Info
  | Warn
  | Error
  | Fatal

type PointValue =
  | Event of template:string
  | Gauge of value:int64 * units:string

type EpochNanoSeconds = int64

module DateTime =
  [<Extension>]
  let timestamp (dt : DateTime) : EpochNanoSeconds =
    dt.Ticks * 100L

module DateTimeOffset =
  [<Extension>]
  let timestamp (dt : DateTimeOffset) : EpochNanoSeconds =
    dt.Ticks * 100L

type Message =
  { name      : string[]
    value     : PointValue
    fields    : Map<string, obj>
    timestamp : EpochNanoSeconds
    level     : LogLevel }

  member x.utcTicks =
    x.timestamp / 100L

type Logger =
  abstract member log : LogLevel -> (unit -> Message) -> Async<unit>
  abstract member logSimple : LogLevel -> Message -> unit

module Loggers =

  [<Literal>]
  let FieldExnKey = "exn"

  /// A logger to use for combining a number of other loggers
  type CombiningLogger(otherLoggers : Logger list) =
    let sendToAll level msgFactory =
      async {
        let! _ =
          otherLoggers
          |> List.map (fun l -> l.log level msgFactory)
          |> Async.Parallel
        return ()
      }

    interface Logger with
      member x.log level msgFactory =
        sendToAll level msgFactory

      member x.logSimple level msg =
        sendToAll level (fun () -> msg)
        |> Async.Start

  /// let the ISO8601 love flow
  let internal defaultFormatter (message : Message) =

    let formatLevel (level : LogLevel) =
      "[" + Char.ToUpperInvariant(message.level.ToString().[0]).ToString() + "] "

    let formatInstant (utcTicks : int64) =
      (DateTime(utcTicks, DateTimeKind.Utc).ToString("o")) + ": "

    let formatValue = function
      | Event template ->
        template

      | Gauge (value, units) ->
        sprintf "%O %s" value units

    let formatName (name : string[]) =
      " [" + String.concat "." name + "]"

    let formatExn (fields : Map<string, obj>) =
      match fields |> Map.tryFind FieldExnKey with
      | None ->
        String.Empty

      | Some ex ->
        " exn:\n" + ex.ToString()

    // [I] 2014-04-05T12:34:56Z: Hello World! [my.sample.app]
    formatLevel message.level +
    formatInstant message.utcTicks +
    formatValue message.value +
    formatName message.name +
    formatExn message.fields

  /// Log a line with the given format, printing the current time in UTC ISO-8601 format
  /// and then the string, like such:
  /// '2013-10-13T13:03:50.2950037Z: today is the day'
  type ConsoleWindowLogger(minLevel, ?formatter, ?colourise, ?originalColor, ?consoleSemaphore) =
    let sem           = defaultArg consoleSemaphore (obj())
    let originalColor = defaultArg originalColor Console.ForegroundColor
    let formatter     = defaultArg formatter defaultFormatter
    let colourise     = defaultArg colourise true
    let write         = System.Console.WriteLine : string -> unit

    let toColour = function
      | LogLevel.Verbose -> ConsoleColor.DarkGreen
      | LogLevel.Debug   -> ConsoleColor.Green
      | LogLevel.Info    -> ConsoleColor.White
      | LogLevel.Warn    -> ConsoleColor.Yellow
      | LogLevel.Error   -> ConsoleColor.DarkRed
      | LogLevel.Fatal   -> ConsoleColor.Red

    let log color message =
      if colourise then
        lock sem <| fun _ ->
          Console.ForegroundColor <- color
          (write << formatter) message
          Console.ForegroundColor <- originalColor
      else
        // we don't need to take another lock, since Console.WriteLine does that for us
        (write << formatter) message

    interface Logger with
      member x.log level f =
        if level >= minLevel then
          log (toColour level) (f ())
        async.Return ()

      member x.logSimple level msg =
        if level >= minLevel then
          log (toColour level) msg

  type OutputWindowLogger(minLevel, ?formatter) =
    let formatter = defaultArg formatter defaultFormatter
    let log msg = System.Diagnostics.Debug.WriteLine(formatter msg)
    interface Logger with
      member x.log level msgFactory =
        if level >= minLevel then log (msgFactory ())
        async.Return ()

      member x.logSimple level msg =
        if level >= minLevel then log msg
      
  let saneDefaultsFor level =
    if level >= LogLevel.Warn then
      ConsoleWindowLogger(level) :> Logger
    else
      CombiningLogger(
        [ ConsoleWindowLogger(level)
          OutputWindowLogger(level) ])
      :> Logger