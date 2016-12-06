module Program

open Expecto
open Logary
open Logary.Internals
open Logary.Adapters.Facade
open Hopac
open NodaTime

let stubLogger (minLevel : LogLevel)
               (message : Message ref)
               name =

  { new Logger with
      member x.log level msgFactory =
        x.logWithAck level msgFactory
        |> Alt.afterFun (fun _ -> ())

      member x.logWithAck level messageFactory =
        message := messageFactory level
        Alt.always (Promise (()))

      member x.level =
        minLevel

      member x.name =
        name }

let stubLogManager (message : Message ref) =
  { new LogManager with
      member x.runtimeInfo =
        RuntimeInfo.create "Facade Tests"

      member x.getLoggerAsync name =
        Job.result (stubLogger Verbose message name)

      member x.getLogger name =
        stubLogger Verbose message name

      member x.flushPending dur =
        Alt.always ()

      member x.shutdown fDur sDur =
        Job.result { flushed = Ack
                     stopped = Ack
                     timedOut = false }

      member x.DisposeAsync () =
        Job.result ()

      member x.Dispose() =
        x.DisposeAsync() |> run
  }

[<Tests>]
let tests =
  let assertWorkMessage (msg : Message) =
    Expect.equal msg.level Warn "Should have logged at Warn level"
    Expect.equal msg.value (Event "Hey {user}!") "Should have logged event template"
    let field = msg.fields |> Map.find (PointName.ofSingle "user")
    Expect.equal field (Field (String "haf", None)) "Should have logged user as String"
    Expect.equal msg.timestamp 1470047883029045000L "Should have correct timestamp"

  testList "facades" [
    testList "shared" [
      testProperty "convert string to unit" <| fun (x : Units) ->
        if x = Other "" then true else
        match LoggerAdapterShared.unitOfString (Units.symbol x) with
        | Other _ ->
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
          Expect.equal (!msg).value (Event "Too simplistic") "Should have logged event template"
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
          Expect.equal (!msg).value (Event "A debug log") "Should have logged event template"
      ]
    ]

    testList "v2" [
      testList "logger" [
        let createLoggerSubject () =
          let msg = ref (Message.event Info "empty")
          let stub = stubLogger LogLevel.Info msg (PointName.parse "Libryy.Core")
          LoggerAdapter.createGeneric<Libryy.Logging.Logger> stub,
          msg

        yield testCase "create adapter" <| fun _ ->
          let msg = ref (Message.event Info "empty")
          let stub = stubLogger LogLevel.Info msg (PointName.parse "Libryy.Core")
          let logger = LoggerAdapter.createString "Libryy.Logging.Logger, Libryy" stub
          Expect.isNotNull logger "Should have gotten logger back"

        yield testCase "end to end with adapter, full logWithAck method" <| fun _ ->
          let libryyLogger, msg = createLoggerSubject ()
          let res = Libryy.Core.work libryyLogger
          Expect.equal 42 res "Should get result back"
          assertWorkMessage (!msg)
          Expect.equal (!msg).name (PointName.parse "Libryy.Core.work") "Should have set name"

        yield testCase "end to end with adapter, log method" <| fun _ ->
          let libryyLogger, msg = createLoggerSubject ()
          let res = Libryy.Core.workBackpressure libryyLogger
          Expect.equal 45 res "Should get result back"
          assertWorkMessage (!msg)
          Expect.equal (!msg).name (PointName.parse "Libryy.Core.work") "Should have set name"

        yield testCase "end to end with adapter, errorWithBP method" <| fun _ ->
          let libryyLogger, msg = createLoggerSubject ()
          let res = Libryy.Core.errorWithBP libryyLogger
          Expect.equal 43 res "Should get result back"
          Expect.equal (!msg).level Error "Should have logged at Error level"
          Expect.equal (!msg).value (Event "Too simplistic") "Should have logged event template"
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
          LogaryFacadeAdapter.initialise<Libryy.Logging.Logger> logManager
          let res = Libryy.Core.staticWork () |> Async.RunSynchronously
          Expect.equal res 49 "Should return 49"
          Expect.equal (!msg).level Debug "Should have logged at Debug level"
          Expect.equal (!msg).value (Event "A debug log") "Should have logged event template"
      ]
    ]

    testList "cs" [
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
          Expect.equal (!msg).value (Event "Too simplistic") "Should have logged event template"
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
          Expect.equal (!msg).value (Event "A debug log") "Should have logged event template"
      ]

    ]
  ]
[<EntryPoint>]
let main argv = 
  Tests.runTestsInAssembly defaultConfig argv