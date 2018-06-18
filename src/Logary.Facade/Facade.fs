/// The logging namespace, which contains the logging abstraction for this
/// library. See https://github.com/logary/logary for details. This module is
/// completely stand-alone in that it has no external references and its adapter
/// in Logary has been well tested.
///
/// This facade is Apache licensed unless running on MSFT-based web servers, in which
/// case you need a commercial license: contact henrik@haf.se for one.
namespace Logary.Facade

open System
open System.Runtime.CompilerServices
open Hopac
open Hopac.Infixes

/// Module that contains the 'known' keys of the Maps in the Message type's context.
module Literals =
  /// What version of the Facade is this. This is a major version that allows the Facade
  /// adapter to choose how it handles the API.
  let FacadeVersion = 4u

  /// What language this Facade has. This controls things like naming standards.
  let FacadeLanguage = "F#"

  /// Avoid conflict with user defined context key
  [<Literal>]
  let internal LogaryPrefix = "_logary."

  /// To recognize all fields for generate formatted msg
  [<Literal>]
  let FieldsPrefix = "_fields."

  /// To recognize all gauge fields for generate formatted msg
  [<Literal>]
  let GaugeNamePrefix = LogaryPrefix + "gauge."

  /// All gauges should have this tag. It's added whenever a gauge is added to the message.
  [<Literal>]
  let GaugeTag = "gauge"

  [<Literal>]
  let ErrorsContextName = LogaryPrefix + "errors"

  /// The tags context field
  [<Literal>]
  let TagsContextName =  LogaryPrefix +  "tags"

/// The log level denotes how 'important' the gauge or event message is.
[<CustomEquality; CustomComparison>]
type LogLevel =
  /// The log message is not that important; can be used for intricate debugging.
  | Verbose
  /// The log message is at a default level, debug level. Useful for shipping to
  /// infrastructure that further processes it, but not so useful for human
  /// inspection in its raw format, except during development.
  | Debug
  /// The log message is informational; e.g. the service started, stopped or
  /// some important business event occurred.
  | Info
  /// The log message is a warning; e.g. there was an unhandled exception or
  /// an even occurred which was unexpected. Sometimes human corrective action
  /// is needed.
  | Warn
  /// The log message is at an error level, meaning an unhandled exception
  /// occurred at a location where it is deemed important to keeping the service
  /// running. A human should take corrective action.
  | Error
  /// The log message denotes a fatal error which cannot be recovered from. The
  /// service should be shut down. Human corrective action is needed.
  | Fatal

  /// Converts the LogLevel to a string
  override x.ToString () =
    match x with
    | Verbose -> "verbose"
    | Debug   -> "debug"
    | Info    -> "info"
    | Warn    -> "warn"
    | Error   -> "error"
    | Fatal   -> "fatal"

  /// Turn the LogLevel into an integer
  member x.toInt () =
    (function
    | Verbose -> 1
    | Debug   -> 2
    | Info    -> 3
    | Warn    -> 4
    | Error   -> 5
    | Fatal   -> 6) x

  interface IComparable<LogLevel> with
    member x.CompareTo other = compare (x.toInt()) (other.toInt())
  static member op_LessThan (a, b) = (a :> IComparable<LogLevel>).CompareTo(b) < 0
  static member op_LessThanOrEqual (a, b) = (a :> IComparable<LogLevel>).CompareTo(b) <= 0
  static member op_GreaterThan (a, b) = (a :> IComparable<LogLevel>).CompareTo(b) > 0
  static member op_GreaterThanOrEqual (a, b) = (a :> IComparable<LogLevel>).CompareTo(b) >= 0
  override x.GetHashCode () = x.toInt ()
  interface IComparable with
    member x.CompareTo other =
      match other with
      | null ->
        1
      | :? LogLevel as tother ->
        (x :> IComparable<LogLevel>).CompareTo tother
      | _ ->
        failwithf "Invalid comparison %A to %A" x other
  interface IEquatable<LogLevel> with
    member x.Equals other = x.toInt() = other.toInt()
  override x.Equals other =
    (x :> IComparable).CompareTo other = 0

/// The # of nanoseconds after 1970-01-01 00:00:00.
type EpochNanoSeconds = int64

/// Allows you to clearly deliniate the accuracy and type of the measurement/gauge.
type Value =
  /// A CLR Double / F# float represented as a DU case
  | Float of float
  /// A CLR Int64 / F# int64 represented as a DU case
  | Int64 of int64
  | BigInt of bigint
  | Fraction of int64 * int64

  /// Convert the Gauge value to a float (best as possible; this **may** lead to
  /// a loss of accuracy).
  member x.toFloat () =
    match x with
    | Float f -> f
    | Int64 i -> float i
    | BigInt i -> float i
    | Fraction (n, d) -> float n / float d

type Units =
  // E.g. to denote nano-seconds since epoch;
  // 1474139353507070000 would be Scaled(Seconds, 10.**9.) since year 1970
  // so to get back to seconds, you'd divide the value by 10.**9.
  // E.g. an op that takes 5ms would be represented as
  // Gauge(5000000, Scaled(Seconds, 10.**9.)) (ns) OR:
  // Gauge(50000, Scaled(Seconds, 10**7.)) (ticks):
  | Scaled of unit:Units * value:float
  | Seconds
  | Scalar
  | Other of unit:string
  // reflection-workarounds for non-stable field names on different F# versions (???):
  member x.scaledFloat = match x with Scaled (_, f) -> f | _ -> 0.
  member x.scaledUnit = match x with Scaled (u, _) -> u | _ -> failwith "Only available on Scaled"
  member x.otherUnit = match x with Other s -> s | _ -> null

/// Time calculation constants
module Constants =
  /// BCL ticks. Not the same as Stopwatch ticks.
  [<Literal>]
  let SecondsPerTick = 0.0000001
  [<Literal>]
  let MillisPerTick = 0.0001
  [<Literal>]
  let MicrosPerTick = 0.1
  [<Literal>]
  let NanosPerTick = 100L
  [<Literal>]
  let NanosPerMicro = 1_000L
  [<Literal>]
  let NanosPerMilli = 1_000_000L
  [<Literal>]
  let NanosPerSecond = 1_000_000_000L
  [<Literal>]
  let NanosPerMinute = 60_000_000_000L
  [<Literal>]
  let MicrosPerSecond = 1_000_000L
  [<Literal>]
  let MillisPerSecond = 1_000L
  /// BCL ticks. Not the same as Stopwatch ticks.
  [<Literal>]
  let TicksPerMinute = 600_000_000L
  /// BCL ticks. Not the same as Stopwatch ticks.
  [<Literal>]
  let TicksPerSecond = 10_000_000L
  /// BCL ticks. Not the same as Stopwatch ticks.
  [<Literal>]
  let TicksPerMilli = 10000L
  /// BCL ticks. Not the same as Stopwatch ticks.
  [<Literal>]
  let TicksPerMicro = 10L

[<Struct>]
type Gauge =
  Gauge of Value * Units
with
  member x.value =
    let (Gauge (v, _)) = x in v
  member x.unit =
    let (Gauge (_, u)) = x in u
  static member ofNanos (ns: Value) =
    Gauge (ns, Scaled (Seconds, float Constants.NanosPerSecond))
  static member ofNanos (ns: int64) =
    Gauge (Int64 ns, Scaled (Seconds, float Constants.NanosPerSecond))
  static member ofNanos (ns: float) =
    Gauge (Float ns, Scaled (Seconds, float Constants.NanosPerSecond))
  static member ofMillis (ms: Value) =
    Gauge (ms, Scaled (Seconds, float Constants.MillisPerSecond))
  static member ofMillis (ms: float) =
    Gauge (Float ms, Scaled (Seconds, float Constants.MillisPerSecond))
  static member ofMillis (ms: int64) =
    Gauge (Int64 ms, Scaled (Seconds, float Constants.MillisPerSecond))
  static member ofBclTicks (bclTicks: Value) =
    Gauge (bclTicks, Scaled (Seconds, float Constants.TicksPerSecond))
  static member ofBclTicks (bclTicks: int64) =
    Gauge (Int64 bclTicks, Scaled (Seconds, float Constants.TicksPerSecond))
  static member ofBclTicks (bclTicks: float) =
    Gauge (Float bclTicks, Scaled (Seconds, float Constants.TicksPerSecond))
  static member ofStopwatchTicks (swTicks: Value) =
    Gauge (swTicks, Scaled (Seconds, float System.Diagnostics.Stopwatch.Frequency))
  static member ofStopwatchTicks (swTicks: int64) =
    Gauge (Int64 swTicks, Scaled (Seconds, float System.Diagnostics.Stopwatch.Frequency))
  static member ofStopwatchTicks (swTicks: float) =
    Gauge (Float swTicks, Scaled (Seconds, float System.Diagnostics.Stopwatch.Frequency))

type StacktraceLineData =
  { site: string
    file: string option
    lineNo: int option }
  static member create site file lineNo =
    { site = site; file = file; lineNo = lineNo }

/// http://geekswithblogs.net/BlackRabbitCoder/archive/2012/01/12/c.net-little-pitfalls-stopwatch-ticks-are-not-timespan-ticks.aspx
type StopwatchTicks = int64

/// http://geekswithblogs.net/BlackRabbitCoder/archive/2012/01/12/c.net-little-pitfalls-stopwatch-ticks-are-not-timespan-ticks.aspx
module StopwatchTicks =
  open System.Diagnostics
  let inline getTimestamp (): StopwatchTicks =
    Stopwatch.GetTimestamp()
  let ticksPerNanosecond =
    // [ tick / s ] / [ ns / s ] => [ tick / ns ]
    float Stopwatch.Frequency / 1_000_000_000.
  let toNanoseconds (ticks: StopwatchTicks) =
    // e.g. 1 tick / 0.01 = 100 ns
    int64 (float ticks / ticksPerNanosecond)
  let toTimeSpan (ticks: StopwatchTicks) =
    TimeSpan.FromTicks ((ticks * TimeSpan.TicksPerSecond) / Stopwatch.Frequency)

[<AutoOpen>]
module DateTimeOffsetEx =
  type DateTimeOffset with
    /// Get the Logary timestamp off the DateTimeOffset.
    member x.toTimestamp(): EpochNanoSeconds =
      (x.Ticks - DateTimeOffset(1970, 1, 1, 0, 0, 0, TimeSpan.Zero).Ticks)
      * 100L

module DateTimeOffset =
  /// Get the DateTimeOffset ticks from EpochNanoSeconds, at offset +0000.
  let ofTimestamp (epoch: EpochNanoSeconds): DateTimeOffset =
    let ticks = epoch / 100L + DateTimeOffset(1970, 1, 1, 0, 0, 0, TimeSpan.Zero).Ticks
    DateTimeOffset(ticks, TimeSpan.Zero)

module Job =
  let timeFun onComplete xJ =
    Job.delay (fun () ->
      let ts = StopwatchTicks.getTimestamp()
      xJ >>- fun x ->
      let now = StopwatchTicks.getTimestamp()
      onComplete (Gauge.ofStopwatchTicks (now - ts))
      x)

  let timeJob onCompleteJ xJ =
    Job.delay (fun () ->
      let ts = StopwatchTicks.getTimestamp()
      xJ >>= fun x ->
      let now = StopwatchTicks.getTimestamp()
      onCompleteJ (Gauge.ofStopwatchTicks (now - ts)) >>- fun () ->
      x)

module Alt =
  let timeFun onComplete onNack (xA: Alt<'a>) =
    Alt.withNackJob (fun nack ->
      let ts = StopwatchTicks.getTimestamp ()
      let markNack =
        nack |> Alt.afterFun (fun () ->
        let now = StopwatchTicks.getTimestamp ()
        onNack (Gauge.ofStopwatchTicks (now - ts)))
      let altCommit =
        xA |> Alt.afterFun (fun x ->
        let now = StopwatchTicks.getTimestamp ()
        onComplete (Gauge.ofStopwatchTicks (now - ts))
        x)
      Job.start markNack >>-. altCommit)

  let timeJob onComplete onNack (xA: Alt<'a>) =
    Alt.withNackJob (fun nack ->
      let ts = StopwatchTicks.getTimestamp ()
      let markNack =
        nack |> Alt.afterJob (fun () ->
        let now = StopwatchTicks.getTimestamp ()
        onNack (Gauge.ofStopwatchTicks (now - ts)))
      let altCommit =
        xA |> Alt.afterJob (fun x ->
        let now = StopwatchTicks.getTimestamp ()
        onComplete (Gauge.ofStopwatchTicks (now - ts))
        >>-. x)
      Job.start markNack >>-. altCommit)

/// Why was the message/metric/event not logged?
[<Struct>]
type LogError =
  /// The buffer of the target was full, so the message was not logged.
  | BufferFull of target:string
  /// The target, or the processing step before the targets, rejected the message.
  | Rejected

type internal LogResult = Alt<Result<Promise<unit>, LogError>>

module internal Promise =
  let unit: Promise<unit> = Promise (())

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module internal LogError =
  let rejected: LogError = Rejected
  let bufferFull target: LogError = BufferFull target

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module internal LogResult =
  let success: Alt<Result<Promise<unit>, LogError>> = Alt.always (Result.Ok Promise.unit)
  let bufferFull target: Alt<Result<Promise<unit>, LogError>> = Alt.always (Result.Error (BufferFull target))
  let rejected: Alt<Result<Promise<unit>, LogError>> = Alt.always (Result.Error Rejected)

module internal H =
  /// Finds all exceptions
  let getExns (context: Map<string, obj>): exn list =
    context
    |> Map.tryFind Literals.ErrorsContextName
    |> Option.map (function
      | :? list<exn> as exns -> exns |> List.filter (isNull >> not)
      | :? list<obj> as exns -> exns |> List.choose (function :? exn as e when not (isNull e) -> Some e | _ -> None)
      | _ -> [])
    |> Option.defaultValue []
    |> List.rev

/// This is record that is logged. It's capable of representing both metrics
/// (gauges) and events. See https://github.com/logary/logary for details.
type Message =
  { /// The 'path' or 'name' of this data point. Do not confuse message template in message.value
    name: string[]
    /// Event (template or raw message) E.g. "{user} logged in"
    value: string
    /// Where in the code? Who did the operation? What tenant did the principal
    /// who did it belong to? ... context can be anything, you can decide how to deal with them in target
    /// through its key.
    context: Map<string, obj>
    /// How important? See the docs on the LogLevel type for details.
    level: LogLevel
    /// When? The # of nanoseconds since the UNIX epoch (1970-01-01T00:00:00Z)
    timestamp: EpochNanoSeconds }

  member x.timestampDateTimeOffset(): DateTimeOffset =
    DateTimeOffset.ofTimestamp x.timestamp

  member x.getFields(): Map<string, obj> =
    x.context
    |> Seq.filter (fun (KeyValue (k, _)) -> k.StartsWith Literals.FieldsPrefix)
    |> Seq.map (fun (KeyValue (k, v)) -> k.Substring(Literals.FieldsPrefix.Length), v)
    |> Map.ofSeq

  /// If you're looking for how to transform the Message's fields, then use the
  /// module methods rather than instance methods, since you'll be creating new
  /// values rather than changing an existing value.
  member __.README =
    ()

type Logger =
  abstract member name: string[]
  abstract member logWithAck: waitForBuffers:bool * level:LogLevel -> (LogLevel -> Message) -> LogResult

module Logger =
  /// Log a message, but don't await all targets to flush. Equivalent to logWithBP.
  /// Returns whether the message was successfully placed in the buffers.
  /// SAFE.
  let log (logger: Logger) logLevel messageFactory: Alt<bool> =
    logger.logWithAck (false, logLevel) messageFactory ^-> function
      | Ok _ ->
        true
      | Result.Error Rejected ->
        true
      | Result.Error (BufferFull _) ->
        false

  let private printDotOnOverflow accepted =
    if not accepted then System.Console.Error.Write '.' else ()

  let logSimple (logger: Logger) msg: unit =
    start (log logger msg.level (fun _ -> msg) ^-> printDotOnOverflow)

  let logWith (logger: Logger) level messageFactory: unit =
    start (log logger level messageFactory ^-> printDotOnOverflow)

  let logWithBP (logger: Logger) logLevel messageFactory: Alt<unit> =
    logger.logWithAck (true, logLevel) messageFactory ^=> function
      | Ok _ ->
        Job.result ()
      | Result.Error Rejected ->
        Job.result ()
      | Result.Error (BufferFull target) ->
        //Job.raises (exn (sprintf "logWithAck (true, _) should have waited for the RingBuffer(s) to accept the Message. Target(%s)" target))
        Job.result ()

  /// Special case: e.g. Fatal messages.
  let logAck (logger: Logger) level messageFactory: Promise<unit> =
    let ack = IVar ()
    let inner =
      logger.logWithAck (true, level) messageFactory ^=> function
        | Ok promise ->
          Job.start (promise ^=> IVar.fill ack)
        | Result.Error Rejected ->
          IVar.fill ack ()
        | Result.Error (BufferFull target) ->
          //let e = exn (sprintf "logWithAck (true, _) should have waited for the RingBuffer(s) to accept the Message. Target(%s)" target)
          //IVar.fillFailure ack e
          IVar.fill ack ()
    start inner
    ack :> Promise<_>

  let apply (transform: Message -> Message) (logger: Logger): Logger =
    { new Logger with
        member x.logWithAck (waitForBuffers, logLevel) messageFactory =
          logger.logWithAck (waitForBuffers, logLevel) (messageFactory >> transform)
        member x.name =
          logger.name }

type LoggingConfig =
  { timestamp: unit -> int64
    getLogger: string[] -> Logger
    consoleSemaphore: obj }

module Literate =
  type LiterateToken =
    | Text
    | Subtext
    | Punctuation
    | LevelVerbose
    | LevelDebug
    | LevelInfo
    | LevelWarning
    | LevelError
    | LevelFatal
    | KeywordSymbol
    | NumericSymbol
    | StringSymbol
    | OtherSymbol
    | NameSymbol
    | MissingTemplateField

  type LiterateOptions =
    { formatProvider: IFormatProvider
      theme: LiterateToken -> ConsoleColor
      getLogLevelText: LogLevel -> string
      printTemplateFieldNames: bool }

    static member create ?formatProvider =
      // note: literate is meant for human consumption, and so the default
      // format provider of 'Current' is appropriate here. The reader expects
      // to see the dates, numbers, currency, etc formatted in the local culture
      { formatProvider = defaultArg formatProvider Globalization.CultureInfo.CurrentCulture
        getLogLevelText = function
                | Debug ->    "DBG"
                | Error ->    "ERR"
                | Fatal ->    "FTL"
                | Info ->     "INF"
                | Verbose ->  "VRB"
                | Warn ->     "WRN"
        theme = function
                | Text -> ConsoleColor.White
                | Subtext -> ConsoleColor.Gray
                | Punctuation -> ConsoleColor.DarkGray
                | LevelVerbose -> ConsoleColor.DarkGray
                | LevelDebug -> ConsoleColor.Gray
                | LevelInfo -> ConsoleColor.White
                | LevelWarning -> ConsoleColor.Yellow
                | LevelError -> ConsoleColor.Red
                | LevelFatal -> ConsoleColor.Red
                | KeywordSymbol -> ConsoleColor.Blue
                | NumericSymbol -> ConsoleColor.Magenta
                | StringSymbol -> ConsoleColor.Cyan
                | OtherSymbol -> ConsoleColor.Green
                | NameSymbol -> ConsoleColor.Gray
                | MissingTemplateField -> ConsoleColor.Red
        printTemplateFieldNames = false }

    static member createInvariant() =
      LiterateOptions.create Globalization.CultureInfo.InvariantCulture

module internal FsMtParser =
  open System.Text

  type Property(name: string, format: string) =
    static let emptyInstance = Property("", null)
    static member empty = emptyInstance
    member x.name = name
    member x.format = format
    member internal x.AppendPropertyString(sb: StringBuilder, ?replacementName) =
      sb.Append("{")
        .Append(defaultArg replacementName name)
        .Append(match x.format with null | "" -> "" | _ -> ":" + x.format)
        .Append("}")
    override x.ToString() = x.AppendPropertyString(StringBuilder()).ToString()

  module internal ParserBits =

    let inline isNull o =
      match o with
      | null -> true
      | _ -> false

    let inline isLetterOrDigit c = System.Char.IsLetterOrDigit c
    let inline isValidInPropName c = c = '_' || System.Char.IsLetterOrDigit c
    let inline isValidInFormat c = c <> '}' && (c = ' ' || isLetterOrDigit c || System.Char.IsPunctuation c)
    let inline isValidCharInPropTag c = c = ':' || isValidInPropName c || isValidInFormat c

    [<Struct>]
    type Range(startIndex: int, endIndex: int) =
      member inline x.start = startIndex
      member inline x.``end`` = endIndex
      member inline x.length = (endIndex - startIndex) + 1
      member inline x.getSubstring (s: string) = s.Substring(startIndex, x.length)
      member inline x.isEmpty = startIndex = -1 && endIndex = -1
      static member inline substring (s: string, startIndex, endIndex) = s.Substring(startIndex, (endIndex - startIndex) + 1)
      static member inline empty = Range(-1, -1)

    let inline tryGetFirstCharInRange predicate (s: string) (range: Range) =
      let rec go i =
        if i > range.``end`` then -1
        else if not (predicate s.[i]) then go (i+1) else i
      go range.start

    let inline tryGetFirstChar predicate (s: string) first =
      tryGetFirstCharInRange predicate s (Range(first, s.Length - 1))

    let inline hasAnyInRange predicate (s: string) (range: Range) =
      match tryGetFirstChar (predicate) s range.start with
      | -1 ->
        false
      | i ->
        i <= range.``end``

    let inline hasAny predicate (s: string) = hasAnyInRange predicate s (Range(0, s.Length - 1))
    let inline indexOfInRange s range c = tryGetFirstCharInRange ((=) c) s range

    let inline tryGetPropInRange (template: string) (within: Range): Property =
      // Attempts to validate and parse a property token within the specified range inside
      // the template string. If the property insides contains any invalid characters,
      // then the `Property.Empty' instance is returned (hence the name 'try')
      let nameRange, formatRange =
        match indexOfInRange template within ':' with
        | -1 ->
          within, Range.empty // no format
        | formatIndex ->
          Range(within.start, formatIndex-1), Range(formatIndex+1, within.``end``) // has format part
      let propertyName = nameRange.getSubstring template
      if propertyName = "" || (hasAny (not<<isValidInPropName) propertyName) then
        Property.empty
      elif (not formatRange.isEmpty) && (hasAnyInRange (not<<isValidInFormat) template formatRange) then
        Property.empty
      else
        let format = if formatRange.isEmpty then null else formatRange.getSubstring template
        Property(propertyName, format)

    let findNextNonPropText (startAt: int) (template: string) (foundText: string->unit): int =
      // Finds the next text token (starting from the 'startAt' index) and returns the next character
      // index within the template string. If the end of the template string is reached, or the start
      // of a property token is found (i.e. a single { character), then the 'consumed' text is passed
      // to the 'foundText' method, and index of the next character is returned.
      let mutable escapedBuilder = Unchecked.defaultof<StringBuilder> // don't create one until it's needed
      let inline append (ch: char) = if not (isNull escapedBuilder) then escapedBuilder.Append(ch) |> ignore
      let inline createStringBuilderAndPopulate i =
        if isNull escapedBuilder then
          escapedBuilder <- StringBuilder() // found escaped open-brace, take the slow path
          for chIndex = startAt to i-1 do append template.[chIndex] // append all existing chars
      let rec go i =
        if i >= template.Length then
          template.Length // bail out at the end of the string
        else
          let ch = template.[i]
          match ch with
          | '{' ->
            if (i+1) < template.Length && template.[i+1] = '{' then
              createStringBuilderAndPopulate i; append ch; go (i+2)
            else i // found an open brace (potentially a property), so bail out
          | '}' when (i+1) < template.Length && template.[i+1] = '}' ->
            createStringBuilderAndPopulate i; append ch; go (i+2)
          | _ ->
            append ch; go (i+1)

      let nextIndex = go startAt
      if (nextIndex > startAt) then // if we 'consumed' any characters, signal that we 'foundText'
        if isNull escapedBuilder then
          foundText (Range.substring(template, startAt, nextIndex - 1))
        else
          foundText (escapedBuilder.ToString())
      nextIndex

    let findPropOrText (start: int) (template: string)
                       (foundText: string -> unit)
                       (foundProp: Property -> unit): int =
      // Attempts to find the indices of the next property in the template
      // string (starting from the 'start' index). Once the start and end of
      // the property token is known, it will be further validated (by the
      // tryGetPropInRange method). If the range turns out to be invalid, it's
      // not a property token, and we return it as text instead. We also need
      // to handle some special case here: if the end of the string is reached,
      // without finding the close brace (we just signal 'foundText' in that case).
      let nextInvalidCharIndex =
        match tryGetFirstChar (not << isValidCharInPropTag) template (start+1) with
        | -1 ->
          template.Length
        | idx ->
          idx

      if nextInvalidCharIndex = template.Length || template.[nextInvalidCharIndex] <> '}' then
        foundText (Range.substring(template, start, (nextInvalidCharIndex - 1)))
        nextInvalidCharIndex
      else
        let nextIndex = nextInvalidCharIndex + 1
        let propInsidesRng = Range(start + 1, nextIndex - 2)
        match tryGetPropInRange template propInsidesRng with
        | prop when not (obj.ReferenceEquals(prop, Property.empty)) ->
          foundProp prop
        | _ ->
          foundText (Range.substring(template, start, (nextIndex - 1)))
        nextIndex

  /// Parses template strings such as "Hello, {PropertyWithFormat:##.##}"
  /// and calls the 'foundTextF' or 'foundPropF' functions as the text or
  /// property tokens are encountered.
  let parseParts (template: string) foundTextF foundPropF =
    let tlen = template.Length
    let rec go start =
      if start >= tlen then () else
      match ParserBits.findNextNonPropText start template foundTextF with
      | next when next <> start ->
        go next
      | _ ->
        go (ParserBits.findPropOrText start template foundTextF foundPropF)
    go 0

/// Internal module for formatting text for printing to the console.
module internal LiterateTokenisation =
  open System.Text
  open Literals
  open Literate

  type TokenisedPart =
    string * LiterateToken

  type LiterateTokeniser =
    LiterateOptions -> Message -> TokenisedPart list

  type internal TemplateToken =
    | TextToken of text:string
    | PropToken of name: string * format: string

  let internal parseTemplate template =
    let tokens = ResizeArray<TemplateToken>()
    let foundText (text: string) = tokens.Add (TextToken text)
    let foundProp (prop: FsMtParser.Property) = tokens.Add (PropToken (prop.name, prop.format))
    FsMtParser.parseParts template foundText foundProp
    tokens

  /// Chooses the appropriate `LiterateToken` based on the value `Type`.
  let tokenForValue (value: obj) =
    match value with
    | :? bool ->
      KeywordSymbol
    | :? int16 | :? int32 | :? int64 | :? decimal | :? float | :? single ->
      NumericSymbol
    | :? string | :? char ->
      StringSymbol
    | _ ->
      OtherSymbol

  let tokeniseValue (options: LiterateOptions) (fields: Map<string, obj>) (template: string) =
    let themedParts = ResizeArray<TokenisedPart>()
    let matchedFields = ResizeArray<string>()
    let foundText (text: string) = themedParts.Add (text, Text)
    let foundProp (prop: FsMtParser.Property) =
      match Map.tryFind prop.name fields with
      | Some propValue ->
        // render using string.Format, so the formatting is applied
        let stringFormatTemplate = prop.AppendPropertyString(StringBuilder(), "0").ToString()
        let fieldAsText = String.Format (options.formatProvider, stringFormatTemplate, [| propValue |])
        let valueTokenType = tokenForValue propValue
        if options.printTemplateFieldNames then
          themedParts.Add ("["+prop.name+"] ", Subtext)
        matchedFields.Add prop.name
        themedParts.Add (fieldAsText, valueTokenType)

      | None ->
        themedParts.Add (prop.ToString(), MissingTemplateField)

    FsMtParser.parseParts template foundText foundProp
    Set.ofSeq matchedFields, (themedParts :> TokenisedPart seq)

  let tokeniseExn (options: LiterateOptions) (ex: exn) =
    let stackFrameLinePrefix = "   at" // 3 spaces
    let monoStackFrameLinePrefix = "  at" // 2 spaces
    use exnLines = new System.IO.StringReader(ex.ToString())
    let rec go lines =
      match exnLines.ReadLine() with
      | null ->
        List.rev lines // finished reading
      | line ->
        if line.StartsWith(stackFrameLinePrefix) || line.StartsWith(monoStackFrameLinePrefix) then
          // subtext
          go ((line, Subtext) :: (Environment.NewLine, Text) :: lines)
        else
          // regular text
          go ((line, Text) :: (Environment.NewLine, Text) :: lines)
    go []

  let tokeniseExns (options: LiterateOptions) message =
    H.getExns message.context
    |> List.collect (tokeniseExn options)

  let tokeniseLogLevel = function
    | Verbose -> LevelVerbose
    | Debug -> LevelDebug
    | Info -> LevelInfo
    | Warn -> LevelWarning
    | Error -> LevelError
    | Fatal -> LevelFatal

  /// Split a structured message up into theme-able parts (tokens), allowing the
  /// final output to display to a user with colours to enhance readability.
  let tokeniseMessage (options: LiterateOptions) (message: Message): TokenisedPart list =
    let formatLocalTime (epoch: EpochNanoSeconds) =
      DateTimeOffset
        .ofTimestamp(epoch)
        .LocalDateTime
        .ToString("HH:mm:ss", options.formatProvider),
      Subtext

    let fields = message.getFields()
    let _, themedMessageParts = message.value |> tokeniseValue options fields
    let themedExceptionParts = tokeniseExns options message

    [ yield "[", Punctuation
      yield formatLocalTime message.timestamp
      yield " ", Subtext
      yield options.getLogLevelText message.level, tokeniseLogLevel message.level
      yield "] ", Punctuation
      yield! themedMessageParts
      if not (isNull message.name) && message.name.Length > 0 then
        yield " ", Subtext
        yield "<", Punctuation
        yield String.concat "." message.name, Subtext
        yield ">", Punctuation
      yield! themedExceptionParts
    ]

module internal Formatting =
  open Literate
  open System.Text

  let formatValue (fields: Map<string, obj>) value =
    let matchedFields, themedParts =
      LiterateTokenisation.tokeniseValue (LiterateOptions.createInvariant()) fields value
    matchedFields, System.String.Concat(themedParts |> Seq.map fst)

  let formatLevel (level: LogLevel) =
    "[" + Char.ToUpperInvariant(level.ToString().[0]).ToString() + "] "

  let formatInstant (ts: DateTimeOffset) =
    (ts.ToString("o")) + ": "

  let formatName (name: string[]) =
    " [" + String.concat "." name + "]"

  let formatExn (e: exn) =
    " exn:\n" + e.ToString()

  let formatExns =
    H.getExns
    >> List.map formatExn
    >> String.concat "\n"

  let formatFields (ignored: Set<string>) (fields: Map<string, obj>) =
    if not (Map.isEmpty fields) then
      fields
      |> Seq.filter (fun (KeyValue (k, _)) ->
        not (ignored |> Set.contains k))
      |> Seq.map (fun (KeyValue (k, v)) ->
        sprintf "\n - %s: %O" k v)
      |> String.concat ""
    else
      ""

  /// let the ISO8601 love flow
  let defaultFormatter (message: Message) =
    let fields = message.getFields()
    let matchedFields, valueString = formatValue fields message.value

    // [I] 2014-04-05T12:34:56Z: Hello World! [my.sample.app]
    formatLevel message.level +
    formatInstant (message.timestampDateTimeOffset()) +
    valueString +
    formatName message.name +
    formatExns message.context +
    formatFields matchedFields fields

/// Assists with controlling the output of the `LiterateConsoleTarget`.
module internal LiterateFormatting =
  open Literate
  open LiterateTokenisation
  open System.Text

  type ColouredTextPart = string * ConsoleColor

  let literateDefaultColourWriter sem (parts: ColouredTextPart list) =
    lock sem <| fun _ ->
      let originalColour = Console.ForegroundColor
      let mutable currentColour = originalColour
      parts |> List.iter (fun (text, colour) ->
        if currentColour <> colour then
          Console.ForegroundColor <- colour
          currentColour <- colour
        Console.Write(text)
      )
      if currentColour <> originalColour then
        Console.ForegroundColor <- originalColour

  [<AutoOpen>]
  module OutputTemplateTokenisers =
    open System.Collections.Generic

    let tokeniseExtraField (options: LiterateOptions) (message: Message) (field: KeyValuePair<string, obj>) =
      seq {
        yield " - ", Punctuation
        yield field.Key, NameSymbol
        yield ": ", Punctuation
        yield System.String.Format(options.formatProvider, "{0}", field.Value), tokenForValue field.Value
      }

    let tokeniseExtraFields (options: LiterateOptions) (message: Message) (templateFieldNames: Set<string>) =
      let extraFields = message.getFields() |> Map.filter (fun key _ -> not (templateFieldNames.Contains key))
      let mutable isFirst = true
      seq {
        for field in extraFields do
          if isFirst then isFirst <- false
          else yield Environment.NewLine, Text
          yield! tokeniseExtraField options message field
      }

    let tokeniseTimestamp format (options: LiterateOptions) (message: Message) =
      let formattedTimestamp =
        message
          .timestampDateTimeOffset()
          .ToLocalTime()
          .ToString(format, options.formatProvider)
      [ formattedTimestamp, Subtext ] :> seq<_>

    let tokeniseTimestampUtc format (options: LiterateOptions) (message: Message) =
      let formattedTimestamp =
        message.timestampDateTimeOffset().ToString(format, options.formatProvider)
      [ formattedTimestamp, Subtext ] :> seq<_>

    let tokeniseMissingField name format =
      seq {
        yield "{", Punctuation
        yield name, MissingTemplateField
        if not (String.IsNullOrEmpty format) then
          yield ":", Punctuation
          yield format, Subtext
        yield "}", Punctuation }

    let tokeniseLogLevel (options: LiterateOptions) (message: Message) =
      seq { yield options.getLogLevelText message.level,
                  LiterateTokenisation.tokeniseLogLevel message.level }

    let tokeniseSource (options: LiterateOptions) (message: Message) =
      seq { yield (String.concat "." message.name), Subtext }

    let tokeniseNewline (options: LiterateOptions) (message: Message) =
      seq { yield Environment.NewLine, Text }

    let tokeniseTab (options: LiterateOptions) (message: Message) =
      seq { yield "\t", Text }

  /// Creates a `LiterateTokeniser` function which can be passed to the `LiterateConsoleTarget`
  /// constructor in order to customise how each log message is rendered. The default template
  /// would be: `[{timestampLocal:HH:mm:ss} {level}] {message}{newline}{exceptions}`.
  /// Available template fields are: `timestamp`, `timestampUtc`, `level`, `source`,
  /// `newline`, `tab`, `message`, `exceptions`. Any misspelled or otheriwese invalid property
  /// names will be treated as `LiterateToken.MissingTemplateField`.
  let tokeniserForOutputTemplate template: LiterateTokeniser =
    let tokens = parseTemplate template
    fun options message ->
      let fields = message.getFields()
      // render the message template first so we have the template-matched fields available
      let matchedFields, messageParts =
        tokeniseValue options fields message.value

      let tokeniseOutputTemplateField fieldName format = seq {
        match fieldName with
        | "timestamp" ->            yield! tokeniseTimestamp format options message
        | "timestampUtc" ->         yield! tokeniseTimestampUtc format options message
        | "level" ->                yield! tokeniseLogLevel options message
        | "source" ->               yield! tokeniseSource options message
        | "newline" ->              yield! tokeniseNewline options message
        | "tab" ->                  yield! tokeniseTab options message
        | "message" ->              yield! messageParts
        | "properties" ->           yield! tokeniseExtraFields options message matchedFields
        | "exceptions" ->           yield! tokeniseExns options message
        | _ ->                      yield! tokeniseMissingField fieldName format
      }

      seq {
        let lastTokenIndex = tokens.Count - 1
        let mutable nextPartsArray: TokenisedPart[] = null
        for index in [0..lastTokenIndex] do
          let token = tokens.[index]
          match token with
          | TextToken text -> yield text, LiterateToken.Punctuation
          | PropToken (name, format) ->
            if index <> lastTokenIndex && name = "newLineIfNext" then
              match tokens.[index + 1] with
              | PropToken (nextName, nextFormat) ->
                // Tokenise the next property now, to determine if it's 'empty'. To avoid doing
                // unnecessary work, we save these tokens ('nextPartsArray') so it can be
                // 'yield'ed on the next iteration.
                nextPartsArray <- tokeniseOutputTemplateField nextName nextFormat |> Seq.toArray
                if nextPartsArray.Length > 0 then
                  yield! tokeniseNewline options message
              | _ ->
                // It's questionable what to do here. It was an invalid output template,
                // because the {newLineIfNext} should only appear immediately prior to some other
                // valid output field. We could `failwith "invalid output template"`?
                ()
            else
              if not (isNull nextPartsArray) then
                yield! nextPartsArray
                nextPartsArray <- null
              else
                yield! tokeniseOutputTemplateField name format
      }
      |> Seq.toList

/// Logs a line in a format that is great for human consumption,
/// using console colours to enhance readability.
/// Sample: [10:30:49 INF] User "AdamC" began the "checkout" process with 100 cart items
type LiterateConsoleTarget(name, minLevel, ?options, ?literateTokeniser, ?outputWriter, ?consoleSemaphore) =
  let sem          = defaultArg consoleSemaphore (obj())
  let options      = defaultArg options (Literate.LiterateOptions.create())
  let tokenise     = defaultArg literateTokeniser LiterateTokenisation.tokeniseMessage
  let colourWriter = defaultArg outputWriter LiterateFormatting.literateDefaultColourWriter sem

  let colouriseThenNewLine message =
    (tokenise options message) @ [Environment.NewLine, Literate.Text]
    |> List.map (fun (s, t) -> s, options.theme(t))

  /// Creates the target with a custom output template. The default `outputTemplate`
  /// is `[{timestampLocal:HH:mm:ss} {level}] {message}{exceptions}`.
  /// Available template fields are: `timestamp`, `timestampUtc`, `level`, `source`,
  /// `newline`, `tab`, `message`, `exceptions`. Any misspelled or otheriwese invalid property
  /// names will be treated as `LiterateToken.MissingTemplateField`.
  new (name, minLevel, outputTemplate, ?options, ?outputWriter, ?consoleSemaphore) =
    let tokeniser = LiterateFormatting.tokeniserForOutputTemplate outputTemplate
    LiterateConsoleTarget(name, minLevel, ?options=options, literateTokeniser=tokeniser, ?outputWriter=outputWriter, ?consoleSemaphore=consoleSemaphore)

  interface Logger with
    member __.name = name
    member __.logWithAck (wfb, level) msgFactory =
      if level >= minLevel then
        Alt.prepareFun (fun () ->
          colourWriter (colouriseThenNewLine (msgFactory level))
          LogResult.success)
      else
        LogResult.success

type TextWriterTarget(name, minLevel, writer: System.IO.TextWriter, ?formatter) =
  let formatter = defaultArg formatter Formatting.defaultFormatter
  let log msg = writer.WriteLine(formatter msg)

  interface Logger with
    member __.name = name
    member __.logWithAck (wfb, level) messageFactory =
      if level >= minLevel then
        Alt.prepareFun (fun () ->
          log (messageFactory level)
          LogResult.success)
      else
        LogResult.success

type OutputWindowTarget(name, minLevel, ?formatter) =
  let formatter = defaultArg formatter Formatting.defaultFormatter
  let log msg = System.Diagnostics.Debug.WriteLine(formatter msg)
  interface Logger with
    member __.name = name
    member __.logWithAck (wfb, level) messageFactory =
      if level >= minLevel then
        Alt.prepareFun (fun () ->
          log (messageFactory level)
          LogResult.success)
      else
        LogResult.success

module Global =
  /// This is the global semaphore for colourising the console output. Ensure
  /// that the same semaphore is used across libraries by using the Logary
  /// Facade Adapter in the final composing app/service.
  let private consoleSemaphore = obj ()

  /// The global default configuration, which logs to Console at Info level.
  let defaultConfig =
    { timestamp = fun () -> DateTimeOffset.UtcNow.toTimestamp()
      getLogger = fun name -> LiterateConsoleTarget(name, Debug) :> Logger
      consoleSemaphore = consoleSemaphore }

  let private config =
    ref (defaultConfig, (* logical clock *) 1u)

  /// The flyweight just references the current configuration. If you want
  /// multiple per-process logging setups, then don't use the static methods,
  /// but instead pass a Logger instance around, setting the name field of the
  /// Message value you pass into the logger.
  type internal Flyweight(name: string[]) =
    let updating = obj()
    let mutable fwClock: uint32 = snd !config
    let mutable logger: Logger = (fst !config).getLogger name
    let rec withLogger action =
      let cfg, cfgClock = !config // copy to local
      let fwCurr = fwClock // copy to local
      if cfgClock <> fwCurr then
        lock updating <| fun _ ->
          logger <- cfg.getLogger name
          fwClock <- fwCurr + 1u
      action logger

    let ensureName (m: Message) =
      if Array.isEmpty m.name then { m with name = name } else m

    interface Logger with
      member x.name = name
      member x.logWithAck (wfb, level) msgFactory =
        withLogger (fun logger -> logger.logWithAck (wfb, level) (msgFactory >> ensureName))

  let internal getStaticLogger (name: string []) =
    Flyweight name

  let getTimestamp (): EpochNanoSeconds =
    (fst !config).timestamp ()

  /// Returns the synchronisation object to use when printing to the console.
  let internal getConsoleSemaphore () =
    (fst !config).consoleSemaphore

  /// Run the passed function under the console semaphore lock.
  let internal lockSem fn =
    lock (getConsoleSemaphore ()) fn

  /// Call from the initialisation of your library. Initialises the
  /// Logary.Facade globally/per process.
  let initialise cfg =
    config := (cfg, snd !config + 1u)

  let initialiseIfDefault cfg =
    if snd !config = 1u then initialise cfg

/// "Shortcut" for creating targets; useful at the top-level configuration point of
/// your library.
module Targets =
  /// Create a new target. Prefer `Log.create` in your own libraries, or let the
  /// composing app replace your target instance through your configuration.
  ///
  /// Will log to console (colourised) by default, and also to the output window
  /// in your IDE if you specify a level below Info.
  let create level name =
    LiterateConsoleTarget(name, level, consoleSemaphore = Global.getConsoleSemaphore())
    :> Logger

/// Module for acquiring static loggers (when you don't want or can't)
/// pass loggers as values.
module Log =
  /// Create a named logger. Full stop (.) acts as segment delimiter in the
  /// hierachy of namespaces and loggers.
  let create (name: string) =
    if name = null then invalidArg "name" "name is null"
    Global.getStaticLogger (name.Split([|'.'|], StringSplitOptions.RemoveEmptyEntries))
    :> Logger

  /// Create an hierarchically named logger
  let createHiera (name: string[]) =
    if name = null then invalidArg "name" "name is null"
    if name.Length = 0 then invalidArg "name" "must have >0 segments"
    Global.getStaticLogger name
    :> Logger

/// The Message module contains functions that can help callers compose messages. This
/// module is especially helpful to open to make calls into Logary's facade small.
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Message =
  open Literals
  open System.Diagnostics

  let setContext name value (message: Message) =
    if isNull name then message
    else { message with context = message.context |> Map.add name (box value) }

  let tryGetContext name (message: Message): 'a option =
    if isNull name then None else
    message.context
      |> Map.tryFind name
      |> Option.bind (function
        | :? 'a as x -> Some x
        | x when isNull x -> Some (Unchecked.defaultof<obj> :?> 'a)
        | _ -> None)

  let getContextByPrefix (prefix: string) (message: Message): seq<_> =
    if String.IsNullOrEmpty prefix then
      Map.toSeq message.context
    else
      message.context
      |> Seq.filter (fun (KeyValue (k, _)) -> k.StartsWith prefix)
      |> Seq.map (fun (KeyValue (k, v)) -> k.Substring(prefix.Length), v)

  let setField name value (message: Message): Message =
    { message with context = message.context |> Map.add (Literals.FieldsPrefix + name) (box value) }

  let tryGetField name (message: Message): 'a option =
    tryGetContext (Literals.FieldsPrefix + name) message

  let getAllFields message =
    getContextByPrefix Literals.FieldsPrefix message

  let getAllTags message =
    tryGetContext Literals.TagsContextName message
    |> Option.defaultValue Set.empty

  /// Tag the message
  let tag (tag: string) (message: Message) =
    let tags = message |> getAllTags |> Set.add tag
    setContext Literals.TagsContextName tags message

  ///////////////// CTORS ////////////////////

  let create context level name value =
    let timestamp = Global.getTimestamp ()
    { context = context
      level = level
      name = name
      value = value
      timestamp = timestamp }

  /// Create a new event log message.
  let event level value =
    create Map.empty level Array.empty value

  /// Create a new event log message – like `event` but with parameters flipped.
  /// E.g. `logger.verbose (eventX "Returned {code}" >> setField "code" 24)`
  let eventX template level =
    event level template

  let addGauge gaugeName (gauge: Gauge) (message: Message) =
    let gaugeName = Literals.GaugeNamePrefix + gaugeName
    message |> setContext gaugeName gauge |> tag Literals.GaugeTag

  let gaugeWithUnit sensorName gaugeName gauge =
    create Map.empty LogLevel.Debug sensorName String.Empty
    |> addGauge gaugeName gauge

  let gauge sensorName gaugeName value =
    gaugeWithUnit sensorName gaugeName (Gauge (value, Units.Scalar))

  /// Check if the Message has a tag
  let hasTag (tag: string) (message: Message) =
    message |> getAllTags |> Set.contains tag

  /// Sets the name/path of the log message.
  let setName (name: string[]) (x: Message) =
    { x with name = name }

  /// Sets the final portion o fthe name of the Message.
  let setNameEnding (ending: string) (x: Message) =
    if String.IsNullOrWhiteSpace ending then x else
    let segs = ResizeArray<_>(x.name)
    segs.Add ending
    { x with name = segs.ToArray() }

  /// Sets the name as a single string; if this string contains dots, the string
  /// will be split on these dots.
  let setSingleName (name: string) (x: Message) =
    if name = null then invalidArg "name" "may not be null"

    let splitName =
      name.Split([|'.'|], StringSplitOptions.RemoveEmptyEntries)

    x |> setName splitName

  /// Sets the message value / template / message
  let setEvent value (m: Message): Message =
    { m with value = value }

  /// Sets the timestamp on the log message.
  let setTimestamp (ts: EpochNanoSeconds) (x: Message) =
    { x with timestamp = ts }

  /// Sets the level on the log message.
  let setLevel (level: LogLevel) (x: Message) =
    { x with level = level }

  /// Adds an exception to the Message, to the 'errors' field, inside a list.
  let addExn (e: exn) msg =
    let exns =
      match tryGetContext Literals.ErrorsContextName msg with
      | Some exns ->
        e :: exns
      | _ ->
        e :: []
    setContext Literals.ErrorsContextName exns msg

  let getExns (msg: Message) = H.getExns msg.context

  let addCallerInfo (memberName, path, line) msg =
    match memberName, path, line with
    | Some m, p, l ->
      let data = StacktraceLineData.create m p l
      msg |> setField "callerInfo" data
    | None, _, _ ->
      msg

/// Syntactic sugar on top of Logger for F# libraries.
[<AutoOpen>]
module LoggerEx =
  let inline private nullIsNone x =
    match x with
    | null -> None
    | x -> Some x

  type Logger with
    member x.log logLevel (messageFactory: LogLevel -> Message): Alt<bool> =
      Logger.log x logLevel messageFactory

    member x.logSimple message: unit =
      Logger.logSimple x message

    member x.logWith level messageFactory: unit =
      Logger.logWith x level messageFactory

    member x.logWithBP level messageFactory: Alt<unit> =
      Logger.logWithBP x level messageFactory

    member x.logAck level messageFactory: Promise<unit> =
      Logger.logAck x level messageFactory

    member x.apply transform: Logger =
      Logger.apply transform x

    member x.verbose (messageFactory: LogLevel -> Message): unit =
      Logger.logWith x Verbose messageFactory

    member x.verboseWithBP (messageFactory: LogLevel -> Message): Alt<unit> =
      Logger.logWithBP x Verbose messageFactory

    member x.verboseWithAck (messageFactory: LogLevel -> Message): Promise<unit> =
      Logger.logAck x Verbose messageFactory

    member x.debug (messageFactory: LogLevel -> Message): unit =
      Logger.logWith x Debug messageFactory

    member x.debugWithBP (messageFactory: LogLevel -> Message): Alt<unit> =
      Logger.logWithBP x Debug messageFactory

    member x.debugWithAck (messageFactory: LogLevel -> Message): Promise<unit> =
      Logger.logAck x Debug messageFactory

    member x.info messageFactory: unit =
      Logger.logWith x Info messageFactory

    member x.infoWithBP messageFactory: Alt<unit> =
      Logger.logWithBP x Info messageFactory

    member x.infoWithAck (messageFactory: LogLevel -> Message): Promise<unit> =
      Logger.logAck x Info messageFactory

    member x.warn messageFactory: unit =
      Logger.logWith x Warn messageFactory

    member x.warnWithBP messageFactory: Alt<unit> =
      Logger.logWithBP x Warn messageFactory

    member x.warnWithAck (messageFactory: LogLevel -> Message): Promise<unit> =
      Logger.logAck x Warn messageFactory

    member x.error messageFactory: unit =
      Logger.logWith x Error messageFactory

    member x.errorWithBP messageFactory: Alt<unit> =
      Logger.logWithBP x Error messageFactory

    member x.errorWithAck (messageFactory: LogLevel -> Message): Promise<unit> =
      Logger.logAck x Error messageFactory

    member x.fatal messageFactory: unit =
      Logger.logWith x Fatal messageFactory

    member x.fatalWithBP messageFactory: Alt<unit> =
      Logger.logWithBP x Fatal messageFactory

    member x.fatalWithAck (messageFactory: LogLevel -> Message): Promise<unit> =
      Logger.logAck x Fatal messageFactory

    member x.timeFun (f: 'input -> 'res,
                      ?measurement: string,
                      ?transform: Message -> Message,
                      ?waitForAck: bool,
                      ?logBefore: bool,
                      [<CallerMemberName>] ?memberName: string,
                      [<CallerFilePath>] ?path: string,
                      [<CallerLineNumber>] ?line: int)
                      : 'input -> 'res =
      let measurement = measurement |> Option.bind nullIsNone |> Option.orElse memberName |> Option.defaultValue "time"
      let transform = defaultArg transform id
      let waitForAck = defaultArg waitForAck false
      let logBefore = defaultArg logBefore false
      fun input ->
        if logBefore then
          x.verbose (Message.eventX "Before {measurement}" >> Message.setField "measurement" measurement)
        let ts = StopwatchTicks.getTimestamp()
        let res = f input
        let dur = Gauge.ofStopwatchTicks (ts - StopwatchTicks.getTimestamp())
        let cb dur =
          fun level ->
            dur
            |> Message.gaugeWithUnit x.name measurement
            |> Message.setLevel level
            |> Message.addCallerInfo (memberName, path, line)
            |> transform
        let logged =
          if waitForAck then
            x.debugWithAck (cb dur)
          else
            x.debug (cb dur)
            Promise.unit

        while not (Promise.Now.isFulfilled logged) do
          System.Threading.Thread.Sleep(5)

        res

    member x.timeJob (xJ: Job<'a>,
                      ?measurement: string,
                      ?transform: Message -> Message,
                      ?waitForAck: bool,
                      ?logBefore: bool,
                      [<CallerMemberName>] ?memberName: string,
                      [<CallerFilePath>] ?path: string,
                      [<CallerLineNumber>] ?line: int)
                      : Job<'a> =
      let measurement = measurement |> Option.bind nullIsNone |> Option.orElse memberName |> Option.defaultValue "time"
      let transform = defaultArg transform id
      let waitForAck = defaultArg waitForAck false
      let logBefore = defaultArg logBefore false
      let cb dur =
        fun level ->
          dur
          |> Message.gaugeWithUnit x.name measurement
          |> Message.setLevel level
          |> Message.addCallerInfo (memberName, path, line)
          |> transform
      let onComplete dur =
        if waitForAck then x.logAck Debug (cb dur) :> Job<_>
        else x.log Debug (cb dur) |> Job.Ignore
      let timedJob =
        Job.timeJob onComplete xJ
      if logBefore then
        x.log Verbose (Message.eventX "Before {measurement}" >> Message.setField "measurement" measurement)
        >>=. timedJob
      else
        timedJob

    member x.timeAlt (xA: Alt<'a>,
                      ?measurement: string,
                      ?transform: Message -> Message,
                      ?waitForAck: bool,
                      ?logBefore: bool,
                      [<CallerMemberName>] ?memberName: string,
                      [<CallerFilePath>] ?path: string,
                      [<CallerLineNumber>] ?line: int)
                      : Alt<'a> =
      let measurement = measurement |> Option.bind nullIsNone |> Option.orElse memberName |> Option.defaultValue "time"
      let transform = defaultArg transform id
      let waitForAck = defaultArg waitForAck false
      let logBefore = defaultArg logBefore false
      let cb wasNacked dur =
        fun level ->
          Message.gaugeWithUnit x.name measurement dur
          |> Message.tag (if wasNacked then "nack" else "ack")
          |> Message.setLevel level
          |> Message.addCallerInfo (memberName, path, line)
          |> transform
      let onComplete dur =
        if waitForAck then x.logAck Debug (cb true dur) :> Job<_>
        else x.log Debug (cb true dur) |> Job.Ignore
      let onNack dur =
        if waitForAck then x.logAck Debug (cb false dur) :> Job<_>
        else x.log Debug (cb false dur) |> Job.Ignore
      let timedAlt =
        Alt.timeJob onComplete onNack xA
      if logBefore then
        Alt.prepareJob (fun () ->
          x.log Verbose (Message.eventX "Before {measurement}" >> Message.setField "measurement" measurement)
          >>-. timedAlt)
      else timedAlt
