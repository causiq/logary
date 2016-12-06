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
    Expect.equal msg.name (PointName.parse "Libryy.Core.work") "Should have set name"

  testList "facades" [
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

        yield testCase "end to end with adapter, log method" <| fun _ ->
          let libryyLogger, msg = createLoggerSubject ()
          let res = Libryy.CoreV1.workNonAsync libryyLogger
          Expect.equal 45 res "Should get result back"
          assertWorkMessage (!msg)

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

        yield testCase "end to end with adapter, log method" <| fun _ ->
          let libryyLogger, msg = createLoggerSubject ()
          let res = Libryy.Core.workBackpressure libryyLogger
          Expect.equal 45 res "Should get result back"
          assertWorkMessage (!msg)

        yield testCase "end to end with adapter, logSimple method" <| fun _ ->
          let libryyLogger, msg = createLoggerSubject ()
          let res = Libryy.Core.simpleWork libryyLogger
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
          let res = Libryy.Core.staticWork ()
          Expect.equal res 49 "Should return 49"
          Expect.equal (!msg).level Debug "Should have logged at Debug level"
          Expect.equal (!msg).value (Event "A debug log") "Should have logged event template"
      ]
    ]
  ]
[<EntryPoint>]
let main argv = 
  Tests.runTestsInAssembly defaultConfig argv
