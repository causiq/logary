module Program

open Fuchu
open ExpectoPatronum
open Logary
open Logary.Adapters.Facade
open Hopac

let stubLogger (minLevel : LogLevel)
               (message : Message ref)
               name =

  { new Logger with
      member x.logVerboseWithAck fac =
        x.logWithAck (fac ())

      member x.logDebugWithAck fac =
        x.logWithAck (fac ())

      member x.logWithAck msg =
        message := msg
        Alt.always (Promise.Now.withValue ())

      member x.logSimple msg =
        message := msg

      member x.level =
        minLevel

      member x.name =
        PointName.parse name }

[<Tests>]
let tests =
  let createSubject () =
    let msg = ref (Message.event Info "empty")
    let stub = stubLogger LogLevel.Info msg "Libryy.Core"

    LogaryFacadeAdapter.createGeneric<Libryy.Logging.Logger> stub,
    msg

  testList "facade" [
    testCase "create adapter" <| fun _ ->
      let msg = ref (Message.event Info "empty")
      let stub = stubLogger LogLevel.Info msg "Libryy.Core"
      let logger = LogaryFacadeAdapter.createString "Libryy.Logging.Logger, Libryy" stub
      Expect.isNotNull logger "Should have gotten logger back"

    testCase "end to end with adapter, full log method" <| fun _ ->
      let libryyLogger, msg = createSubject ()
      let res = Libryy.Core.work libryyLogger
      Expect.equal 42 res "Should get result back"
      Expect.equal (!msg).level Warn "Should have logged at Warn level"
      Expect.equal (!msg).value (Event "Hey {user}!") "Should have logged event template"
      let field = (!msg).fields |> Map.find (PointName.ofSingle "user")
      Expect.equal field (Field (String "haf", None)) "Should have logged user as String"
      Expect.equal (!msg).timestamp 1470047883029045000L "Should have correct timestamp"
      Expect.equal (!msg).name (PointName.parse "Libryy.Core.work") "Should have set name"

    testCase "end to end with adapter, logSimple method" <| fun _ ->
      let libryyLogger, msg = createSubject ()
      let res = Libryy.Core.simpleWork libryyLogger
      Expect.equal 43 res "Should get result back"
      Expect.equal (!msg).level Error "Should have logged at Error level"
      Expect.equal (!msg).value (Event "Too simplistic") "Should have logged event template"
      Expect.notEqual (!msg).timestamp 0L "Should have non-zero timestamp"
      Expect.notEqual (!msg).name (PointName [||]) "Should have non-empty point name"
      Expect.equal (!msg).name (PointName [| "Libryy"; "Core" |])
                   "Should have point name corresponding to the passed logger"

    testCase "end to end with adapter, logWithAck method" <| fun _ ->
      ()
  ]

[<EntryPoint>]
let main argv = 
  Tests.defaultMainThisAssembly argv