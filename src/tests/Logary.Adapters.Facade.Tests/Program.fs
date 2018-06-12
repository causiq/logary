module Program

open Expecto
open Logary
open Logary.Configuration
open Logary.Adapters.Facade
open Hopac
open NodaTime
open System

type BaseLogger(minLevel, name, result, ?message: Message ref) =
  let m = defaultArg message (ref (Message.event Info "EMPTY"))
  member x.message = m
  interface Logger with
    member x.logWithAck (_, level) messageFactory =
      m := messageFactory level
      result
    member x.name = name
    member x.level = minLevel

let stubLogger (minLevel: LogLevel) (message: Message ref) name =
  BaseLogger(minLevel, name, Alt.always (Ok (Promise (()))), message)
  :> Logger

let stubLoggerReject name =
  BaseLogger(Debug, name, Alt.always (Result.Error LogError.Rejected))
  :> Logger

let stubLoggerFull name targetName =
  BaseLogger(Debug, name, Alt.always (Result.Error (LogError.BufferFull targetName)))
  :> Logger

let stubLogManager (message: Message ref) =
  { new LogManager with
      member x.runtimeInfo =
        Internals.RuntimeInfo.create "Facade Tests" "localhost" :> _
      member x.getLogger name =
        stubLogger Verbose message name
      member x.getLoggerWithMiddleware name mid =
        stubLogger Verbose message name
      member x.flushPending dur =
        Alt.always (FlushInfo([],[]))
      member x.shutdown () = Alt.always ()
      member x.flushPending () = Alt.always ()
      member x.shutdown (fDur,sDur) =
        Alt.always (FlushInfo([],[]), ShutdownInfo([],[]))
      member x.switchLoggerLevel (path, minLevel) = ()
  }

[<Tests>]
let tests =
  let assertWorkMessage (msg: Message) =
    Expect.equal msg.level Warn "Should have logged at Warn level"
    Expect.equal msg.value ("Hey {user}!") "Should have logged event template"
    let userName = msg |> Message.tryGetField "user"
    Expect.equal userName (Some "haf") "Should have logged user as String"
    Expect.equal msg.timestamp 1470047883029045000L "Should have correct timestamp"

  testList "facades" [
    testList "shared" [
      testProperty "convert string to unit" <| fun (x: Units) ->
        match x with
        | Other _ -> true
        | x -> match Units.parse x.symbol with
               | Other _ -> // consider doing assert = x instead
                 true
               | res ->
                 res = x
    ]

    testList "v1" [
      testList "logger" [
        let createLoggerSubject () =
          let msg = ref (Message.event Info "empty")
          let stub = stubLogger LogLevel.Info msg (PointName.parse "Libryy.Core")
          LoggerAdapter.createGeneric<Libryy.LoggingV1.Logger> stub,
          msg

        yield testCase "create adapter" <| fun _ ->
          let msg = ref (Message.event Info "empty")
          let stub = stubLogger LogLevel.Info msg (PointName.parse "Libryy.Core")
          let logger = LoggerAdapter.createString "Libryy.LoggingV1.Logger, Libryy" stub
          Expect.isNotNull logger "Should have gotten logger back"

        yield testCase "end to end with adapter, full logWithAck method" <| fun _ ->
          let libryyLogger, msg = createLoggerSubject ()
          let res = Libryy.CoreV1.work libryyLogger
          Expect.equal 42 res "Should get result back"
          assertWorkMessage (!msg)
          Expect.equal (!msg).name (PointName.parse "Libryy.Core.work") "Should have set name"

        yield testCase "end to end with adapter, log method" <| fun _ ->
          let libryyLogger, msg = createLoggerSubject ()
          let res = Libryy.CoreV1.workNonAsync libryyLogger
          Expect.equal 45 res "Should get result back"
          assertWorkMessage (!msg)
          Expect.equal (!msg).name (PointName.parse "Libryy.Core.work") "Should have set name"

        yield testCase "end to end with adapter, logSimple method" <| fun _ ->
          let libryyLogger, msg = createLoggerSubject ()
          let res = Libryy.CoreV1.simpleWork libryyLogger
          Expect.equal 43 res "Should get result back"
          Expect.equal (!msg).level Error "Should have logged at Error level"
          Expect.equal (!msg).value ("Too simplistic") "Should have logged event template"
          Expect.notEqual (!msg).timestamp 0L "Should have non-zero timestamp"
          Expect.notEqual (!msg).name (PointName [||]) "Should have non-empty point name"
          Expect.equal (!msg).name (PointName [| "Libryy"; "Core" |])
                       "Should have point name corresponding to the passed logger"
      ]

      testList "global config" [
        let createLogManagerSubject () =
          let msg = ref (Message.event Info "empty")
          stubLogManager msg, msg

        yield testCase "initialise with LogManager" <| fun _ ->
          let logManager, msg = createLogManagerSubject ()
          LogaryFacadeAdapter.initialise<Libryy.LoggingV1.Logger> logManager
          let res = Libryy.CoreV1.staticWork ()
          Expect.equal res 49 "Should return 49"
          Expect.equal (!msg).level Debug "Should have logged at Debug level"
          Expect.equal (!msg).value ("A debug log") "Should have logged event template"
      ]
    ]

    testList "C# v2" [
      testList "logger" [
        let createLoggerSubject () =
          let msg = ref (Message.event Info "empty")
          let stub = stubLogger LogLevel.Info msg (PointName.parse "Cibryy.Core")
          LoggerCSharpAdapter.createGeneric<Cibryy.Logging.ILogger> stub,
          msg

        yield testCase "create adapter" <| fun _ ->
          let msg = ref (Message.event Info "empty")
          let stub = stubLogger LogLevel.Info msg (PointName.parse "Cibryy.Core")
          let logger = LoggerAdapter.createString "Cibryy.Logging.ILogger, Cibryy" stub
          Expect.isNotNull logger "Should have gotten logger back"

        yield testCase "end to end with adapter, full LogWithAck method" <| fun _ ->
          let cibryyLogger, msg = createLoggerSubject ()
          let res = Cibryy.Core.Work cibryyLogger
          Expect.equal 42 res "Should get result back"
          assertWorkMessage (!msg)
          let actual = (!msg).name
          let expected = PointName.parse "Cibryy.Core.Work"
          Expect.equal actual expected "Should have set name"

        yield testCase "end to end with adapter, Log method" <| fun _ ->
          let cibryyLogger, msg = createLoggerSubject ()
          let res = Cibryy.Core.WorkBackpressure cibryyLogger
          Expect.equal 45 res "Should get result back"
          assertWorkMessage (!msg)
          let actual = (!msg).name
          let expected = PointName.parse "Cibryy.Core.WorkBackpressure"
          Expect.equal actual expected "Should have set name"

        yield testCase "end to end with adapter, ErrorWithBP method" <| fun _ ->
          let cibryyLogger, msg = createLoggerSubject ()
          let res = Cibryy.Core.ErrorWithBP cibryyLogger
          Expect.equal 43 res "Should get result back"
          Expect.equal (!msg).level Error "Should have logged at Error level"
          Expect.equal (!msg).value ( "Too simplistic") "Should have logged event template"
          Expect.notEqual (!msg).timestamp 0L "Should have non-zero timestamp"
          Expect.notEqual (!msg).name (PointName [||]) "Should have non-empty point name"
          Expect.equal (!msg).name (PointName [| "Cibryy"; "Core" |])
                       "Should have point name corresponding to the passed logger"
      ]

      testList "global config" [
        let createLogManagerSubject () =
          let msg = ref (Message.event Info "empty")
          stubLogManager msg, msg

        yield testCase "using ILoggerConfig" <| fun _ ->
          let logManager, msg = createLogManagerSubject ()
          let cfgT = System.Type.GetType "Cibryy.Logging.ILoggingConfig, Cibryy"
          Expect.isNotNull cfgT "Should find ILoggingConfig"
          let loggerT = System.Type.GetType "Cibryy.Logging.ILogger, Cibryy"
          Expect.isNotNull loggerT "Should find ILogger"
          let cfg = LogaryFacadeAdapter.createCSharpConfig cfgT loggerT logManager
          Expect.isNotNull cfg "Should return a non-null cfg"
          let ts = cfgT.GetMethod("GetTimestamp").Invoke(cfg, [||]) :?> int64
          Expect.notEqual ts 0L "Should be non-zero"
          let l = cfgT.GetMethod("GetLogger").Invoke(cfg, [| [|"A"; "B"|] |])
          Expect.isNotNull l "Should return a non-null logger"

        yield testCase "initialise with LogManager" <| fun _ ->
          let logManager, msg = createLogManagerSubject ()
          LogaryFacadeAdapter.initialise<Cibryy.Logging.ILogger> logManager
          let res = Cibryy.Core.StaticWork().Result
          Expect.equal res 49 "Should return 49"
          Expect.equal (!msg).level Debug "Should have logged at Debug level"
          Expect.equal (!msg).value ("A debug log") "Should have logged event template"
      ]
    ]

    testList "v3" [
      testList "logger" [
        let createLoggerSubject () =
          let msg = ref (Message.event Info "empty")
          let stub = stubLogger LogLevel.Info msg (PointName.parse "Libryy.CoreV3")
          LoggerAdapter.createGeneric<Libryy.LoggingV3.Logger> stub,
          msg

        yield testCase "create adapter" <| fun _ ->
          let msg = ref (Message.event Info "empty")
          let stub = stubLogger LogLevel.Info msg (PointName.parse "Libryy.CoreV3")
          let logger = LoggerAdapter.createString "Libryy.LoggingV3.Logger, Libryy" stub
          Expect.isNotNull logger "Should have gotten logger back"

        yield testCase "end to end with adapter, full logWithAck method" <| fun _ ->
          let libryyLogger, msg = createLoggerSubject ()
          let res = Libryy.CoreV3.work libryyLogger
          Expect.equal 42 res "Should get result back"
          assertWorkMessage (!msg)
          Expect.equal (!msg).name (PointName.parse "Libryy.Core.work") "Should have set name"

        yield testCase "end to end with adapter, log method" <| fun _ ->
          let libryyLogger, msg = createLoggerSubject ()
          let res = Libryy.CoreV3.workBackpressure libryyLogger
          Expect.equal 45 res "Should get result back"
          assertWorkMessage (!msg)
          Expect.equal (!msg).name (PointName.parse "Libryy.Core.work") "Should have set name"

        yield testCase "end to end with adapter, errorWithBP method" <| fun _ ->
          let libryyLogger, msg = createLoggerSubject ()
          let res = Libryy.CoreV3.errorWithBP libryyLogger
          Expect.equal 43 res "Should get result back"
          Expect.equal (!msg).level Error "Should have logged at Error level"
          Expect.equal (!msg).value ( "Too simplistic") "Should have logged event template"
          Expect.notEqual (!msg).timestamp 0L "Should have non-zero timestamp"
          Expect.notEqual (!msg).name (PointName [||]) "Should have non-empty point name"
          Expect.equal (!msg).name (PointName [| "Libryy"; "CoreV3" |])
                       "Should have point name corresponding to the passed logger"

        yield testCase "with exns" <| fun _ ->
          let libryyLogger, msg = createLoggerSubject ()
          let res = Libryy.CoreV3.generateAndLogExn libryyLogger
          let exns = Message.getExns !msg
          Expect.equal 2 exns.Length "Has two exns"
      ]

      testList "global config" [
        let createLogManagerSubject () =
          let msg = ref (Message.event Info "empty")
          stubLogManager msg, msg

        yield testCase "initialise with LogManager" <| fun _ ->
          let logManager, msg = createLogManagerSubject ()
          LogaryFacadeAdapter.initialise<Libryy.LoggingV3.Logger> logManager
          let res = Libryy.CoreV3.staticWork () |> Async.RunSynchronously
          Expect.equal res 49 "Should return 49"
          Expect.equal (!msg).level Debug "Should have logged at Debug level"
          Expect.equal (!msg).value ( "A debug log") "Should have logged event template"
      ]

      // input:Facade Gauge, output; Logary gauge message
      testList "gauge" [
        let loggerType = typeof<Libryy.LoggingV3.Logger>

        let namedGauge name value units: Libryy.LoggingV3.Message =
          { name      = name
            value     = Libryy.LoggingV3.Gauge (value, units)
            fields    = Map.empty
            timestamp = Libryy.LoggingV3.Global.timestamp ()
            level     = Libryy.LoggingV3.Debug }

        let expectGauge name value units (msg: Message) =
          let g = msg.context |> HashMap.tryFind name
          Expect.isSome g "gauge is in context"

          match g.Value with
          | :? Gauge as gauge ->
            let (Gauge (v, u)) = gauge
            Expect.equal v value "has correct value"
            Expect.equal u units "has correct units"
          | _ ->
            Expect.equal true false "Wrong type of gauge object"

        let subject = LoggerAdapter.toMsgV3 Reflection.ApiVersion.V3 (loggerType, [||])

        yield testCase "Seconds" <| fun _ ->
          namedGauge [|"response"; "lag"|] 0.12 "Seconds"
          |> subject
          |> expectGauge "_logary.gauge.lag" (Float 0.12) Units.Seconds

        yield testCase "s" <| fun _ ->
          namedGauge [|"response"; "lag"|] 0.12 "s"
          |> subject
          |> expectGauge "_logary.gauge.lag" (Float 0.12) Units.Seconds

        yield testCase "Milliseconds" <| fun _ ->
          namedGauge [|"response"; "lag"|] 120.0 "Milliseconds"
          |> subject
          |> expectGauge "_logary.gauge.lag" (Float 120.0) (Units.Scaled (Units.Seconds, 1000.0))

        yield testCase "ms" <| fun _ ->
          namedGauge [|"response"; "lag"|] 120.0 "ms"
          |> subject
          |> expectGauge "_logary.gauge.lag" (Float 120.0) (Units.Scaled (Units.Seconds, 1000.0))

        yield testCase "µs" <| fun _ ->
          namedGauge [|"response"; "lag"|] 120.0 "µs"
          |> subject
          |> expectGauge "_logary.gauge.lag" (Float 120.0) (Units.Scaled (Units.Seconds, 1.0e6))

        yield testCase "Nanoseconds" <| fun _ ->
          namedGauge [|"response"; "lag"|] 120.0 "Nanoseconds"
          |> subject
          |> expectGauge "_logary.gauge.lag" (Float 120.0) (Units.Scaled (Units.Seconds, 1.0e9))

        yield testCase "ns" <| fun _ ->
          namedGauge [|"response"; "lag"|] 120.0 "ns"
          |> subject
          |> expectGauge "_logary.gauge.lag" (Float 120.0) (Units.Scaled (Units.Seconds, 1.0e9))

        yield testCase "unknown turns is Units.Other" <| fun _ ->
          namedGauge [|"response"; "lag"|] 120.0 "Moments"
          |> subject
          |> expectGauge "_logary.gauge.lag" (Float 120.0) (Units.Other "Moments")

        yield testCase "default name" <| fun _ ->
          namedGauge [||] 120.0 ""
          |> subject
          |> expectGauge "_logary.gauge.default-gauge" (Float 120.0) Units.Scalar
      ]
    ]

    testList "v4" [
      yield testList "logger" [
        let createLoggerSubject () =
          let msg = ref (Message.event Info "empty")
          let stub = stubLogger LogLevel.Info msg (PointName.parse "Libryy.CoreV4")
          LoggerAdapter.createGeneric<Libryy.LoggingV4.Logger> stub,
          msg

        yield testCase "create adapter" <| fun _ ->
          let msg = ref (Message.event Info "empty")
          let stub = stubLogger LogLevel.Info msg (PointName.parse "Libryy.CoreV4")
          let logger = LoggerAdapter.createString "Libryy.LoggingV4.Logger, Libryy" stub
          Expect.isNotNull logger "Should have gotten logger back"

        yield testCase "end to end with adapter, full logWithAck method" <| fun _ ->
          let libryyLogger, msg = createLoggerSubject ()
          let res = Libryy.CoreV4.work libryyLogger
          Expect.equal 42 res "Should get result back"
          assertWorkMessage (!msg)
          Expect.equal (!msg).name (PointName.parse "Libryy.Core.work-work-work") "Should have set name"

        yield testCaseJob "end to end with rejection result" <| job {
          let properLogger = stubLoggerReject (PointName.parse "Adapters.Facade.v4")
          let subject = LoggerAdapter.createGeneric<Libryy.LoggingV4.Logger> properLogger
          let! res = subject.logWithAck (false, Libryy.LoggingV4.LogLevel.Warn) (Libryy.LoggingV4.Message.eventX "Hello world")
          res |> Flip.Expect.equal "Should eq rejected" (Result.Error Libryy.LoggingV4.LogError.Rejected)
        }

        yield testCaseJob "end to end with buffer full result" <| job {
          let properLogger = stubLoggerFull (PointName.parse "Adapters.Facade.v4") "t1"
          let subject = LoggerAdapter.createGeneric<Libryy.LoggingV4.Logger> properLogger
          let! res = subject.logWithAck (false, Libryy.LoggingV4.LogLevel.Warn) (Libryy.LoggingV4.Message.eventX "Hello world")
          res |> Flip.Expect.equal "Should eq rejected" (Result.Error (Libryy.LoggingV4.LogError.BufferFull "t1"))
        }

        yield testCase "end to end with adapter, log method" <| fun _ ->
          let libryyLogger, msg = createLoggerSubject ()
          let res = Libryy.CoreV4.workBackpressure libryyLogger
          Expect.equal 45 res "Should get result back"
          assertWorkMessage (!msg)
          Expect.equal (!msg).name (PointName.parse "Libryy.Core.work-bp") "Should have set name"

        yield testCase "end to end with adapter, errorWithBP method" <| fun _ ->
          let libryyLogger, msg = createLoggerSubject ()
          let res = Libryy.CoreV4.errorWithBP libryyLogger
          Expect.equal 43 res "Should get result back"
          Expect.equal (!msg).level Error "Should have logged at Error level"
          Expect.equal (!msg).value ( "Too simplistic") "Should have logged event template"
          Expect.notEqual (!msg).timestamp 0L "Should have non-zero timestamp"
          Expect.notEqual (!msg).name (PointName [||]) "Should have non-empty point name"
          Expect.equal (!msg).name (PointName [| "Libryy"; "CoreV4" |])
                       "Should have point name corresponding to the passed logger"

        yield testCase "with exns" <| fun _ ->
          let libryyLogger, msg = createLoggerSubject ()
          let res = Libryy.CoreV4.generateAndLogExn libryyLogger
          let exns = Message.getExns !msg
          Expect.equal 2 exns.Length "Has two exns"
      ]

      yield testList "global config" [
        let createLogManagerSubject () =
          let msg = ref (Message.event Info "empty")
          stubLogManager msg, msg

        yield testCase "initialise with LogManager" <| fun _ ->
          let logManager, msg = createLogManagerSubject ()
          LogaryFacadeAdapter.initialise<Libryy.LoggingV4.Logger> logManager
          let res = Libryy.CoreV4.staticWork () |> run
          Expect.equal res 49 "Should return 49"
          Expect.equal (!msg).level Debug "Should have logged at Debug level"
          Expect.equal (!msg).value ( "A debug log") "Should have logged event template"
      ]

      let loggerType = typeof<Libryy.LoggingV4.Logger>

      let v4float f =
        Libryy.LoggingV4.Float f

      let v4seconds =
        Libryy.LoggingV4.Seconds

      let v4scalar =
        Libryy.LoggingV4.Scalar

      let v4other str =
        Libryy.LoggingV4.Other str

      let v4scaled baseUnit scale =
        Libryy.LoggingV4.Scaled (baseUnit, scale)

      let v4gauge v u =
        Libryy.LoggingV4.Gauge (v, u)

      yield testList "units" [
        let values =
          [ v4scaled v4seconds 1e3, Scaled (Seconds, 1e3)
            v4seconds, Seconds
            v4other "xs", Other "xs"
          ]

        for source, target in values do
          let sourceType = source.GetType().FullName
          yield testCase (sprintf "convert %s => %A" sourceType target) <| fun () ->
            LoggerAdapter.toUnits loggerType source
            |> Flip.Expect.equal "Should convert successfully" target
      ]

      // input:Facade Gauge, output; Logary gauge message
      yield testList "gauge" [

        let namedGauge sensor name value units: Libryy.LoggingV4.Message =
          Libryy.LoggingV4.Gauge (Libryy.LoggingV4.Float value, units)
          |> Libryy.LoggingV4.Message.gaugeWithUnit sensor name

        let gaugeMessage gauge: Libryy.LoggingV4.Message =
          Libryy.LoggingV4.Message.gaugeWithUnit [|"container"|] "startup" gauge

        let expectGauge name value units (msg: Message) =
          let n = Libryy.LoggingV4.Literals.GaugeNamePrefix + name
          let g = msg.context |> HashMap.tryFind n
          Expect.isSome g "gauge is in context"

          let gauge = g.Value :?> Gauge
          let (Gauge (v, u)) = gauge
          Expect.equal v value "has correct value"
          Expect.equal u units "has correct units"

        let subjectToLogary = LoggerAdapter.toMsgV4 (loggerType, [||])

        yield testCase "Seconds" <| fun _ ->
          v4gauge (v4float 0.12) v4seconds
          |> gaugeMessage
          |> subjectToLogary
          |> expectGauge "startup" (Float 0.12) Units.Seconds

        yield testCase "Milliseconds" <| fun _ ->
          v4gauge (v4float 120.0) (v4scaled v4seconds 1e3)
          |> gaugeMessage
          |> subjectToLogary
          |> expectGauge "startup" (Float 120.) (Units.Scaled (Units.Seconds, 1e3))

        yield testCase "Nanoseconds" <| fun _ ->
          v4gauge (v4float 120.0) (v4scaled v4seconds 1e9)
          |> gaugeMessage
          |> subjectToLogary
          |> expectGauge "startup" (Float 120.0) (Units.Scaled (Units.Seconds, 1e9))
      ]
    ]
  ]

[<EntryPoint>]
let main argv =
  Tests.runTestsInAssembly defaultConfig argv