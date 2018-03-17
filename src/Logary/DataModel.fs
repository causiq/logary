namespace Logary

open Hopac
open NodaTime
open System
open Logary

/// A content-type annotation for a byte-array.
type ContentType = string

/// The main alias for time in Logary - the # of nanoseconds since 1970-01-01
/// in Unix time. In effect it denotes the # of nanoseconds passed in
/// international atomic time (TAI), but corrected for leap seconds – because
/// this is how system clocks normally work.
type EpochNanoSeconds = int64

[<Obsolete ("Use .Net objects instead. This type is reserve for API backwards compatibility.")>]
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

type Gauge = Gauge of float * Units

[<Obsolete ("Use (Message.setField : string -> 'a -> Message -> Message) directly, just for api compatibility")>]
type Field =
  Field of Value * Units option

/// This is record that is logged.
type Message =
  { /// The 'path' or 'name' of this data point. Do not confuse message template in message.value
    name: PointName
    /// Event (template or raw message) E.g. "{user} logged in"
    value: string
    /// Where in the code? Who did the operation? What tenant did the principal
    /// who did it belong to? ... context can be anything, you can decide how to deal with them in target
    /// through its key.
    context: HashMap<string, obj>
    /// How important? See the docs on the LogLevel type for details.
    level: LogLevel
    /// When? nanoseconds since UNIX epoch.
    timestamp: EpochNanoSeconds }

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

  /// Gets the currently set log level (minimal,inclusive),
  /// aka. the granularity with which things are being logged.
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