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
  | Other of unit:string
  // E.g. to denote nano-seconds since epoch;
  // 1474139353507070000 would be Scaled(Seconds, 10.**9.) since year 1970
  // so to get back to seconds, you'd divide the value by 10.**9.
  // E.g. an op that takes 5ms would be represented as
  // Gauge(5000000, Scaled(Seconds, 10.**9.)) (ns) OR:
  // Gauge(50000, Scaled(Seconds, 10**7.)) (ticks):
  | Scaled of unit:Units * value:float
  | Offset of unit:Units * value:float
  | Mul of unitA:Units * unitB:Units
  | Pow of ``base``:Units * power:Units
  | Div of nom:Units * denom:Units
  | Root of ``base``:Units
  | Log10 of ``base``:Units // Log of base:float * BaseUnit

  /// E.g. 5 degrees celsius is (5 + 273.15) K
  static member Celsius = Offset (Kelvins, +273.15)

  /// 5 min = 5 / (1/60) seconds = 360 s
  static member Minutes = Scaled (Seconds, 1. / 60.)
  static member Hours = Scaled (Seconds, 1. / 3600.)
  static member Days = Scaled (Seconds, 1. / (24. * 3600.))

  member x.name: string option =
    match x with
    | Bits ->
      Some "bits"
    | Bytes ->
      Some "bytes"
    | Seconds ->
      Some "seconds"
    | Metres ->
      Some "metres"
    | Scalar ->
      Some "units"
    | Amperes ->
      Some "amperes"
    | Kelvins ->
      Some "kelvins"
    | Moles ->
      Some "moles"
    | Candelas ->
      Some "candelas"
    | Percent ->
      Some "percent"
    | Watts ->
      Some "watts"
    | Hertz ->
      Some "hertz"
    | Other u ->
      Some u
    | Scaled _
    | Offset _
    | Mul _
    | Pow _
    | Div _
    | Root _
    | Log10 _ ->
      None

  member x.symbol =
    match x with
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
    | Scaled (units, scale) -> sprintf "%s/%f" units.symbol scale
    | Offset (units, offset) ->
      sprintf "%s %s %f" units.symbol (if offset < 0. then "-" else "+") offset
    | Mul (a, b) -> String.Concat [ "("; a.symbol; "*"; b.symbol; ")" ]
    | Pow (a, b) -> String.Concat [ a.symbol; "^("; b.symbol; ")" ]
    | Div (a, b) -> String.Concat [ a.symbol; "/"; b.symbol ]
    | Root a -> String.Concat [ "sqrt("; a.symbol; ")" ]
    | Log10 a -> String.Concat [ "log10("; a.symbol; ")" ]

[<Struct>]
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

  let (|Empty|_|) (pn: PointName) =
    if pn.isEmpty then Some () else None

  [<CompiledName "OfSingle">]
  let ofSingle (segment: string) =
    PointName [| segment |]

  [<CompiledName "OfList">]
  let ofList (hiera: string list) =
    PointName (Array.ofList hiera)

  /// Creates a point name of the array. As a performance optimisation, Logary
  /// assumes that you do not change the array contents afterwards, and will
  /// treat this value like an immutable value.
  [<CompiledName "OfArray">]
  let ofArray (hiera: string[]) =
    PointName hiera

  [<CompiledName "Parse">]
  let parse (s: string) =
    String.splita '.' s |> ofArray

  [<CompiledName "Format">]
  let format (pn: PointName) =
    pn.ToString()

  [<CompiledName "SetEnding">]
  let setEnding (nameEnding: string) (PointName segments as original) =
    if String.IsNullOrWhiteSpace nameEnding then original else
    PointName (Array.append segments [| nameEnding |])

/// Allows you to clearly deliniate the accuracy and type of the measurement/gauge.
type Value =
  /// A CLR Double / F# float represented as a DU case
  | Float of float
  /// A CLR Int64 / F# int64 represented as a DU case
  | Int64 of int64
  | BigInt of bigint
  | Fraction of int64 * int64
  //| Binary of byte[] * ContentType
  /// Convert the Gauge value to a float (best as possible; this **may** lead to
  /// a loss of accuracy).
  member x.toFloat () =
    match x with
    | Float f -> f
    | Int64 i -> float i
    | BigInt i -> float i
    | Fraction (n, d) -> float n / float d

type Gauge =
  Gauge of Value * Units
with
  member x.value =
    let (Gauge (v, _)) = x in v
  member x.unit =
    let (Gauge (_, u)) = x in u

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
    /// When? The # of nanoseconds since the UNIX epoch (1970-01-01T00:00:00Z)
    timestamp: EpochNanoSeconds }

    /// Gets the timestamp as NodaTime ticks (100 ns per tick). If you're getting
    /// for DateTime and/or DateTimeOffset, remember that those start at
    /// 0001-01-01; use the functions on the DateTime/DateTimeOffset modules then instead.
    member x.timestampTicks: int64 =
      x.timestamp / Constants.NanosPerTick
    /// The # of seconds since UNIX epoch 1970-01-01T00:00:00Z
    member x.timestampEpochS: int64 =
      x.timestamp / Constants.NanosPerSecond

/// See the docs on the funtions for descriptions on how Ack works in conjunction
/// with the promise.
type Logger =
  /// The PointName for this `Logger`: corresponds to the `name` field for the
  /// `Messages` produced from this instance.
  abstract name: PointName

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
  abstract logWithAck: LogLevel -> (LogLevel -> Message) -> Alt<Promise<unit>>

  /// Gets the currently set log level (minimal,inclusive),
  /// aka. the granularity with which things are being logged.
  abstract level: LogLevel

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
  abstract elapsed: Duration

  /// Call to sub-divide the TimeScope into two spans with the previous span
  /// labelled as the passed string.
  abstract bisect: string -> unit

  /// Call to stop the timer; decide in the passed function what level the
  /// resulting Gauge should have.
  abstract stop : (Duration -> LogLevel) -> Alt<Promise<unit>>