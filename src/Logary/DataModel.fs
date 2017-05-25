namespace Logary

open Hopac
open NodaTime
open System
open System.Runtime.CompilerServices
open System.Runtime.InteropServices
open Logary

/// A content-type annotation for a byte-array.
type ContentType = string

/// The main alias for time in Logary - the # of nanoseconds since 1970-01-01
/// in Unix time. In effect it denotes the # of nanoseconds passed in
/// international atomic time (TAI), but corrected for leap seconds – because
/// this is how system clocks normally work.
type EpochNanoSeconds = int64

/// Logary's structured logging is centered around the `Value` type.
///
/// All objects logged into Logary are translated into a structure such as this
/// one.
///
/// The trade-off between passing all the CLR type information along versus
/// fitting it all into these cases is one of completeness versus convenience
/// in writing targets and computing on the data.
type Value =
  | String of string
  | Bool of bool
  | Float of float
  | Int64 of int64
  | BigInt of bigint
  | Binary of byte [] * ContentType
  | Fraction of int64 * int64
  | Object of HashMap<string, Value>
  | Array of Value list

type Units =
  | Bits
  | Bytes
  | Seconds
  | Metres
  | Scalar
  | Amperes
  | Kelvins
  | Moles
  | Candelas
  | Percent
  | Watts
  | Hertz
  | Other of units:string
  // E.g. to denote nano-seconds since epoch;
  // 1474139353507070000 would be Scaled(Seconds, 10.**9.) since year 1970
  // so to get back to seconds, you'd divide the value by 10.**9.
  // E.g. an op that takes 5ms would be represented as
  // Gauge(5000000, Scaled(Seconds, 10.**9.)) (ns) OR:
  // Gauge(50000, Scaled(Seconds, 10**7.)) (ticks):
  | Scaled of units:Units * scale:float
  | Offset of units:Units * offset:float
  | Mul of Units * Units
  | Pow of Units * Units
  | Div of Units * Units
  | Root of Units
  | Log10 of Units // Log of base:float * BaseUnit

  /// E.g. 5 degrees celsius is (5 + 273.15) K
  static member Celsius =
    Offset (Kelvins, +273.15)

  static member symbol = function
    | Bits -> "bit"
    | Bytes -> "B"
    | Seconds -> "s"
    | Metres -> "m"
    | Scalar -> ""
    | Amperes -> "A"
    | Kelvins -> "K"
    | Moles -> "mol"
    | Candelas -> "cd"
    | Percent -> "%"
    | Watts -> "W"
    | Hertz -> "Hz"
    | Other other -> other
    | Scaled (units, scale) -> sprintf "%s / %f" (Units.symbol units) scale
    | Offset (units, offset) ->
      sprintf "%s %s %f" (Units.symbol units)
                         (if offset < 0. then "-" else "+")
                         offset
    | Mul (a, b) -> String.Concat [ "("; Units.symbol a; "*"; Units.symbol b; ")" ]
    | Pow (a, b) -> String.Concat [ Units.symbol a; "^("; Units.symbol b; ")" ]
    | Div (a, b) -> String.Concat [ Units.symbol a; "/"; Units.symbol b ]
    | Root a -> String.Concat [ "sqrt("; Units.symbol a; ")" ]
    | Log10 a -> String.Concat [ "log10("; Units.symbol a; ")" ]

type PointName =
  PointName of hierarchy:string[]
with
  member x.isEmpty =
    match x with
    | PointName xs when xs.Length = 0 -> true
    | _ -> false

  override x.ToString() =
    let (PointName hiera) = x in String.concat "." hiera

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module PointName =

  let empty = PointName Array.empty

  [<CompiledName "OfSingle">]
  let ofSingle (segment : string) =
    PointName [| segment |]

  [<CompiledName "OfList">]
  let ofList (hiera : string list) =
    PointName (Array.ofList hiera)

  /// Creates a point name of the array. As a performance optimisation, Logary
  /// assumes that you do not change the array contents afterwards, and will
  /// treat this value like an immutable value.
  [<CompiledName "OfArray">]
  let ofArray (hiera : string[]) =
    PointName hiera

  [<CompiledName "Parse">]
  let parse (s: string) =
    String.splita '.' s |> ofArray

  [<CompiledName "Format">]
  let format (pn : PointName) =
    pn.ToString()

  [<CompiledName "SetEnding">]
  let setEnding (nameEnding : string) (PointName segments as original) =
    if isNull nameEnding then original else
    PointName (Array.append segments [| nameEnding |])

type PointValue =
  /// Value at point in time
  | Gauge of Value * Units
  /// Any sort of derived measure
  | Derived of Value * Units
  /// All simple-valued fields' values can be templated into the template string
  /// when outputting the value in the target.
  | Event of template:string

/// Extensions for C#
type PointValue with
  /// Tries to get the gauge value
  member x.TryGetGauge([<Out>] gauge:byref<Tuple<Value, Units>>) =
    match x with
    | Gauge (value, units) ->
      gauge <- new Tuple<Value, Units>(value, units)
      true
    | _ ->
      false

  /// Tries to get the derived value
  member x.TryGetDerived([<Out>] derived:byref<Tuple<Value, Units>>) =
    match x with
    | Derived (value, units) ->
      derived <- new Tuple<Value, Units>(value, units)
      true
    | _ ->
      false

  /// Tries to get the Event template value (log message)
  member x.TryGetEvent([<Out>] template:byref<string>) =
    match x with
    | Event value ->
      template <- value
      true
    | _ ->
      false

type Field =
  Field of Value * Units option

/// This is record that is logged. It's capable of representing both metrics
/// (gauges) and events.
type Message =
  { /// The 'path' or 'name' of this data point. Do not confuse template in
    /// (Event template) = message.value
    ///
    name      : PointName
    /// The main value for this metric or event. Either a Gauge, a Derived or an
    /// Event. (A discriminated union type)
    value     : PointValue
    /// The semantic-logging data.
    fields    : HashMap<PointName, Field>
    /// Where in the code? Who did the operation? What tenant did the principal
    /// who did it belong to? Put things your normally do 'GROUP BY' on in this
    /// Map.
    context   : HashMap<string, Value>
    /// How important? See the docs on the LogLevel type for details.
    level     : LogLevel
    /// When? nanoseconds since UNIX epoch.
    timestamp : EpochNanoSeconds }

    /// Gets the timestamp as NodaTime ticks (100 ns per tick). If you're getting
    /// for DateTime and/or DateTimeOffset, remember that those start at
    /// 0001-01-01.
    member x.timestampTicks : int64 =
      x.timestamp / Constants.NanosPerTick

/// See the docs on the funtions for descriptions on how Ack works in conjunction
/// with the promise.
type Logger =
  /// The PointName for this `Logger`: corresponds to the `name` field for the
  /// `Messages` produced from this instance.
  abstract name : PointName

  /// Write a message to the Logger. The returned value represents the commit
  /// point that Logary has acquired the message. The alternative is always
  /// selectable (through `Alt.always ()` if the logger filtered out the message
  /// due to a Rule).
  ///
  /// If the Message was not filtered through a Rule, but got sent onwards, the
  /// promise is there to denote the ack that all targets have successfully
  /// flushed the message. If you do not commit to the Alt then it will not be
  /// logged.
  ///
  /// If you choose to not await the Promise/Ack it makes no difference, since
  /// it will be garbage collected in the end, whether it's awaited or not.
  abstract logWithAck : LogLevel -> (LogLevel -> Message) -> Alt<Promise<unit>>

  /// Logs with the specified log level with backpressure via Logary's
  /// buffers.
  ///
  /// Calls to this function will block the caller only while executing the
  /// callback (if the level is active).
  ///
  /// The returned async value will yield when the message has been added to
  /// the buffers of Logary.
  ///
  /// You need to start the (cold) Alt value for the logging to happen.
  ///
  /// You should not do blocking/heavy operations in the callback.
  abstract log : LogLevel -> (LogLevel -> Message) -> Alt<unit>

  /// Gets the currently set log level, aka. the granularity with which things
  /// are being logged.
  abstract level : LogLevel

/// A disposable interface to use with `use` constructs and to create child-
/// contexts. Since it inherits Logger, you can pass this scope down into child
/// function calls. This interface should dovetail with how Zipkin/Dapper
/// manages parent/child spans.
type LoggerScope =
  inherit IDisposable
  inherit Logger

/// Should be created from a Time(LogLevel, callerName) call.
type TimeScope =
  inherit LoggerScope
  /// Gets the currently elapsed duration of this time scope scope.
  abstract elapsed : Duration

  /// Call to sub-divide the TimeScope into two spans with the previous span
  /// labelled as the passed string.
  abstract bisect : string -> unit

  /// Call to stop the timer; decide in the passed function what level the
  /// resulting Gauge should have.
  abstract stop : (Duration -> LogLevel) -> Alt<Promise<unit>>