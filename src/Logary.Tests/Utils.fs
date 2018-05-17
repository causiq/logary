[<AutoOpen>]
module Logary.Tests.Utils

open System
open System.IO
open System.Runtime.CompilerServices
open Hopac
open NodaTime
open Expecto
open Expecto.Flip
open Expecto.Logging

let internal logger = Log.create "Logary.Tests.Utils"

let testCaseJob name xJ =
  testCaseAsync name (Job.toAsync xJ)

let ftestCaseJob name xJ =
  ftestCaseAsync name (Job.toAsync xJ)

let ptestCaseJob name xJ =
  ptestCaseAsync name (Job.toAsync xJ)

open Logary
open Logary.Internals
open Logary.Targets
open Logary.Configuration
open Logary.Configuration

let buildTextWriteTarget name =
  let (out, error) = (new StringWriter (), new StringWriter ())
  let twconf = TextWriter.TextWriterConf.create (out, error)
  let twTargetConf = TextWriter.create twconf name
  (out, error, twTargetConf)

let buildLogManager () = job {
  let svc = "svc"
  let host = "localhost"
  let tname = "4test"
  let (out, error, twTargetConf) = buildTextWriteTarget tname
  // let iloggerConf = ILogger.Targets [ twTargetConf ]

  let! logm =
    Config.create svc host
    // |> Config.ilogger iloggerConf
    // |> Config.ilogger (ILogger.Console Error)
    |> Config.target twTargetConf
    |> Config.processing (Events.events |> Events.sink [tname])
    |> Config.disableGlobals
    |> Config.build
  return logm, out, error
}

[<MethodImpl(MethodImplOptions.NoInlining)>]
let innermost () =
  raise (Exception "Bad things going on")

[<MethodImpl(MethodImplOptions.NoInlining)>]
let middleWay () =
  1 + 3 |> ignore
  innermost ()

[<MethodImpl(MethodImplOptions.NoInlining)>]
let withException f =
  try
    middleWay ()
  with e ->
    f e

type Tenant =
  { tenantId: string
    permissions: string }

let exnMsg =
  Message.event Error "Unhandled exception"
  |> Message.setNameStr "Logary.Tests"
  |> Message.setField "tenant" { tenantId = "12345"; permissions = "RWX" }
  |> Message.setContextFromMap (Map
    [ "user", box (Map
        [ "name", box "haf"
          "id", box "deadbeef234567"
        ])
    ])
  |> withException Message.addExn

let timeMessage (nanos: int64) level =
  let value, units = float nanos, Scaled (Seconds, float Constants.NanosPerSecond)
  let name = PointName.parse "A.B.C"
  Message.gaugeWithUnit name "Check" (Gauge (Float value, units))
  |> Message.setLevel level

let gaugeMessage (value: float) level =
  let name = PointName.parse "Revolver"
  Message.gaugeWithUnit name "spin" (Gauge (Float value, (Div (Seconds, Units.Other "revolution"))))
  |> Message.setLevel level

let multiGaugeMessage level =
  Message.event level "Processor.% Idle"
  |> Message.addGauges [
    "Core 1", (Gauge (Fraction (1L, 1000L), Percent))
    "Core 2", (Gauge (Float 0.99, Percent))
    "Core 3", (Gauge (Float 0.473223755, Percent))
  ]
  |> Message.setContext "host" "db-001"
  |> Message.setContext "service" "api-web"

let emptyRuntime =
  memo (
    Config.createInternalTargets (ILogger.LiterateConsole Verbose)
    |> Config.createInternalLogger (RuntimeInfo.create "logary-tests" "dev-machine")
  )

let nanos xs =
  Duration.FromTicks (xs / Constants.NanosPerTick)

let helloWorld =
  Message.eventX "Hello World!"
  >> Message.setTicksEpoch (0L: EpochNanoSeconds)

let helloWorldTS =
  helloWorld
  >> fun m ->
      let now = SystemClock.Instance.GetCurrentInstant()
      { m with value = sprintf "%s @ %O" m.value now }

module Internals =
  type TimeoutResult<'T> =
  | Success of 'T
  | TimedOut

let finaliseJob target =
  Alt.choose [
    Target.shutdown target
    |> Alt.afterJob id
    |> Alt.afterFun Internals.Success

    timeOutMillis 8000
      |> Alt.afterFun (fun _ -> Internals.TimedOut)
  ]

/// Finalise the target and assert it was finalised within 1000 milliseconds
let finalise target =
  finaliseJob target |> Alt.afterFun (function
    | Internals.TimedOut ->
      failtestf "Finalising target timed out: %A" target
    | Internals.Success _ ->
      ())

let logMsgWaitAndShutdown (targetApi: Target.T) (logCallBack: (Message -> Job<unit>) -> #Job<unit>) =
  let logAndWait (message: Message) =
    job {
      do! logger.infoWithBP (Logging.Message.eventX (sprintf "Sending message to Target(%s)" targetApi.name))
      let! res = Target.tryLog targetApi message
      match res with
      | Ok ack ->
        do! logger.infoWithBP (Logging.Message.eventX (sprintf "Waiting for Target(%s) to ACK message" targetApi.name))
        do! ack
        do! logger.infoWithBP (Logging.Message.eventX (sprintf "Target(%s) ACKed message" targetApi.name))
      | Result.Error e ->
        failtestf "%A" e
    }
  let finaliseJob =
    job {
      do! logger.infoWithBP (Logging.Message.eventX "Finalising target")
      do! finalise targetApi
      do! logger.infoWithBP (Logging.Message.eventX "Target finalised!")
    }

  Job.tryFinallyJob (logCallBack logAndWait) finaliseJob

let testLabel label test =
  TestLabel (label, test, FocusState.Normal)

module Expect =
  let private trim (s: string) = if isNull s then s else s.Trim()

  /// Expect (Result.Ok x) and return x, otherwise fail the test.
  let isOk (m: string) (xR: Result<_,_>) =
    match xR with
    | Result.Ok x ->
      x
    | Result.Error e ->
      failtestf "Expected (Ok _), but got (Error %A). %s" e m

  /// Expect the passed float to be a number.
  let isNotNaN f format =
    if Double.IsNaN f then failtestf "%s. Float was the NaN (not a number) value." format

  /// Expect the passed float not to be positive infinity.
  let isNotPositiveInfinity actual format =
    if Double.IsPositiveInfinity actual then failtestf "%s. Float was positive infinity." format

  /// Expect the passed float not to be negative infinity.
  let isNotNegativeInfinity actual format =
    if Double.IsNegativeInfinity actual then failtestf "%s. Float was negative infinity." format

  /// Expect the passed float not to be infinity.
  let isNotInfinity actual format =
    isNotNegativeInfinity actual format
    isNotPositiveInfinity actual format
    // passed via excluded middle

  /// Expect the passed string not to be empty.
  let isNotEmpty (actual: string) format =
    Expect.isNotNull actual format
    if actual.Length = 0 then Tests.failtestf "%s. Should not be empty." format

  let linesEqual (message: string) (expected: string) (actual: string) =
    let sra = new IO.StringReader(actual)
    let sre = new IO.StringReader(expected)
    let mutable cont = true
    let mutable linea = null
    let mutable linee = null
    while cont do
      linea <- trim (sra.ReadLine())
      linee <- trim (sre.ReadLine())
      linea |> Expect.equal "Should equal the expected line" linee
      cont <- not (isNull linea || isNull linee)

  module Json =
    open Logary.Internals.Chiron

    /// Assert and pass through the value.
    let isObjectX message (value: Json) =
      match value with
      | Json.Object nested ->
        nested
      | other ->
        failtestf "Expected Json.Object, but was %A" other

    /// Assert the Json value is a Json.Object.
    let isObject message value =
      isObjectX message value |> ignore

    /// Assert and pass through the found field.
    let hasFieldX message field (value: JsonObject) =
      value
        |> JsonObject.toMap
        |> Map.tryFind field
        |> function
        | None ->
          failtestf "Did not find field '%s' on Json.Object (%A)" field value
        | Some f ->
          f

    /// Assert and pass through the JsonObject value.
    let hasFieldXX message field (value: JsonObject) =
      hasFieldX message field value |> ignore
      value

    /// Assert and pass through the JsonObject value.
    let hasFieldXXf message field callback (value: JsonObject) =
      callback (hasFieldX message field value) |> ignore
      value

    /// Assert the object has a field of the given name.
    let hasField message field value =
      hasFieldX message field value |> ignore
