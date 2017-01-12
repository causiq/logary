module Logary.Tests.Program

open System
open System.Globalization
open System.Threading
open Hopac
open NodaTime
open Expecto
open Expecto.ExpectoFsCheck
open Logary
open Logary.Internals
open Logary.Message
open Logary.Configuration
open FsCheck

type Obj() =
  member x.PropA =
    45
  member x.PropB =
    raise (Exception ("Oh noes, no referential transparency here"))

type Arbs =
  static member HashMap() =
    let nonNullKey = fun (KeyValue (k, _)) -> not (isNull (box k))
    let filter list = List.filter nonNullKey list
    Arb.Default.FsList()
    |> Arb.convert (filter >> HashMap.ofListPair) HashMap.toListPair

  static member Float() =
    Arb.Default.Float()
    |> Arb.filter (fun f -> not <| Double.IsNaN(f) &&      // Not required to repro
                            not <| Double.IsInfinity(f))   // Not requreid to repro 

  //static member Value() =
  //  Arb.Default.Derive()
  //  |> Arb.filter (function | Float f -> not (Double.IsNaN f) | _ -> true)

let fsCheckConfig =
  { Config.Default with
      Arbitrary = [ typeof<Arbs> ]
  }

[<Tests>]
let tests =
  testList "logary" [
    testList "HashMap" [
      testCase "empty and add element" <| fun () ->
        HashMap.empty |> HashMap.add 2 4 |> ignore
    ]

    testList "LogLevel" [
      testProperty "toInt" <| fun (level : LogLevel) ->
        level.toInt() |> ignore

      testProperty "=" <| fun (level : LogLevel) ->
        level = level

      testProperty "<>" <| fun (level : LogLevel) ->
        if level = Info then level = Info else
        level <> Info
    ]

    testList "MessageTemplates" [
      testCase "all the tests" <| fun () ->
        Tests.skiptest "All tests from https://github.com/messagetemplates/messagetemplates-fsharp/tree/master/test/FsMessageTemplates.Tests"
    ]

    testList "KnownLiterals" (
      [ KnownLiterals.ErrorsFieldName, "errors"
        KnownLiterals.ServiceContextName, "service"
        KnownLiterals.HostContextName, "host"
        KnownLiterals.TagsContextName, "tags"
        KnownLiterals.SuppressPointValue, "suppress-point-value"
      ]
      |> List.map (fun (actual, expected) ->
          testCase "ensuring constant '%s'" <| fun () ->
            Expect.equal actual expected "KnownLiteral should not change"
      )
    )

    testList "Constants (floats)" [
      testList "floats" (
        [ Constants.SecondsPerTick, 0.0000001
          Constants.MillisPerTick, 0.0001
          Constants.MicrosPerTick, 0.1
        ]
        |> List.map (fun (actual, expected) ->
            testCase "ensuring constant '%s'" <| fun () ->
              Expect.equal actual expected "Constant should not change"
        )
      )

      testList "int64s" (
        [ Constants.NanosPerTick, 100L
          Constants.NanosPerMicro, 1000L
          Constants.NanosPerMilli, 1000000L
          Constants.NanosPerSecond, 1000000000L
          Constants.NanosPerMinute, 60000000000L
          Constants.TicksPerMinute, 600000000L
          Constants.TicksPerSecond, 10000000L
          Constants.TicksPerMilli, 10000L
          Constants.TicksPerMicro, 10L
        ]
        |> List.map (fun (actual, expected) ->
            testCase "ensuring constant '%s'" <| fun () ->
              Expect.equal actual expected "Constant should not change"
        )
      )
    ]

    testList "Internals" [
      testCase "Seq.any" <| fun () ->
        Tests.skiptest "TBD"

      testCase "Seq.last" <| fun () ->
        Tests.skiptest "TBD"

      testCase "Promise.instapromise" <| fun () ->
        Tests.skiptest "TBD"

      testCase "Alt.apply" <| fun () ->
        Tests.skiptest "TBD"

      testCase "List.Job.apply" <| fun () ->
        Tests.skiptest "TBD"

      testCase "List.traverseJobA" <| fun () ->
        Tests.skiptest "TBD"

      testCase "List.traverseAltA" <| fun () ->
        Tests.skiptest "TBD"
    ]

    testList "Config" [
      testCase "create" <| fun () ->
        Config.create "tests" "hostname-123" |> ignore
    ]

    testList "Value" [
      testCase "String" <| fun () ->
        String "abc" |> ignore
    ]

    testList "Units" [
      testCase "Bits" <| fun () ->
        Bits |> ignore
    ]

    testList "PointName" [
      testCase "empty" <| fun () ->
        Expect.equal PointName.empty (PointName [||]) "Should be empty"

      testCase "c'tor" <| fun () ->
        PointName [| "a"; "b" |] |> ignore

      testCase "setEnding" <| fun () ->
        let pn = PointName.ofSingle "A" |> PointName.setEnding "B"
        Expect.equal pn (PointName.parse "A.B") "Should be equal to A.B.C"

      testCase "setEnding with dots – no parse" <| fun () ->
        let pn = PointName.ofSingle "A" |> PointName.setEnding "B.C"
        Expect.equal pn (PointName [| "A"; "B.C" |]) "Should be equal"
    ]

    testList "Message" [
      testCase "event : LogLevel -> string -> Message" <| fun _ ->
        let m = event Info "Hello world"
        Expect.equal m.level Info "Should have info level"
        Expect.equal m.value (Event "Hello world") "Should have template"

      testCase "eventX : string -> LogLevel -> Message" <| fun _ ->
        let m = eventX "Hello world" Info
        Expect.equal m.level Info "Should have info level"
        Expect.equal m.value (Event "Hello world") "Should have template"

      testCase "setFieldsFromObject : obj -> Message -> Message" <| fun () ->
        Tests.skiptest "Awaiting usage of TypeShape"
        let m = eventX "Hello world" Info |> setFieldsFromObject (Obj())
        let field = m.fields |> HashMap.tryFind (PointName.ofSingle "PropA") |> Option.get
        Expect.equal field (Field (Int64 45L, None)) "Should have PropA"
    ]

    testList "Logger" [
      testCase "public interface" <| fun () ->
        let logger =
          { new Logger with
              member x.name : PointName = PointName.ofSingle "B"
              member x.logWithAck (level : LogLevel) (factory : LogLevel -> Message) : Alt<Promise<unit>> =
                Promise.instaPromise
              member x.log (level : LogLevel) (factory : LogLevel -> Message) : Alt<unit> =
                Alt.always ()
              member x.level : LogLevel =
                Debug
          }
        Expect.equal logger.level Debug "Should have Debug level"
    ]

    testList "LoggerScope" [
      testCase "public interface :> Logger" <| fun () ->
        let hasLogger = typeof<Logger>.IsAssignableFrom(typeof<LoggerScope>)
        Expect.isTrue hasLogger "Should implement Logger"

      testCase "public interface :> IDisposable" <| fun () ->
        let hasIDisposable = typeof<IDisposable>.IsAssignableFrom(typeof<LoggerScope>)
        Expect.isTrue hasIDisposable "Should implement IDisposable"
    ]

    testList "TimeScope" [
      testCase "public interface" <| fun () ->
        { new TimeScope with
            member x.name : PointName = PointName.ofSingle "B"
            member x.logWithAck (level : LogLevel) (factory : LogLevel -> Message) : Alt<Promise<unit>> =
              Promise.instaPromise
            member x.log (level : LogLevel) (factory : LogLevel -> Message) : Alt<unit> =
              Alt.always ()
            member x.level : LogLevel =
              Debug
            member x.Dispose () = ()
            member x.elapsed = Duration.Zero
            member x.bisect (label : string) : unit =
              ()
            member x.stop (decider : Duration -> LogLevel) : Alt<Promise<unit>> =
              Promise.instaPromise
        }
        |> ignore
    ]

    testList "ValueModule" [
      testCase "exceptionToStringValueMap" <| fun () ->
        Tests.skiptest "Use TypeShape"

      testCase "create : 'a -> Value" <| fun () ->
        Tests.skiptest "Use TypeShape"
    ]

    testList "UnitsModule" [
      testList "formatValue" (
        [ String "A", "A"
          Bool true, "true"
          Bool false, "false"
          Float 62., "62"
          Int64 84598L, "84598"
          BigInt 1024I, "1024"
          Binary ([| 254uy |], "application/octet-stream"),
          "hex:FE;content-type:application/octet-stream"
          Fraction (2L, 5L), "2/5"
          Array [ String "B"; String "C" ], "[ B, C ]"

          // TO CONSIDER – Value.Object
          Object HashMap.empty, "Object"
        ]
        |> List.map (fun (value, expected) ->
            testCase "String \"a\"" (fun () ->
              let actual = Units.formatValue value
              Expect.equal actual expected "Should have correct result"
            )
        )
      )

      testCase "scaleBy10" <| fun () ->
        Tests.skiptest "Bring in from master"

      testCase "scaleBytes" <| fun () ->
        Tests.skiptest "Bring in from master"

      testCase "calculate" <| fun () ->
        Tests.skiptest "Bring in from master"

      testCase "scale" <| fun () ->
        Tests.skiptest "Bring in from master"

      testCase "formatWithUnit" <| fun () ->
        Tests.skiptest "Bring in from master"
    ]

    // TO CONSIDER: bring back Scheduling when needed

    testList "NullLogger" [
      testCase "should be named 'Logary.NullLogger'" <| fun () ->
        let sut = NullLogger.instance
        Expect.equal sut.name (PointName.parse "Logary.NullLogger")
                     "Is called Logary.NullLogger"

      testCase "only logs 'Fatal'" <| fun () ->
        let sut = NullLogger.instance
        Expect.equal sut.level Fatal "Logs at Fatal level"

      testCaseAsync "logWithAck returns" (async {
        let sut = NullLogger.instance
        let! p = Alt.toAsync (sut.logWithAck Fatal (eventX "hi"))
        do! Alt.toAsync (p :> Alt<_>)
      })

      testCaseAsync "log returns" (async {
        let sut = NullLogger.instance
        do! Alt.toAsync (sut.log Fatal (eventX "hi"))
      })
    ]

    testList "Middleware" [
      testCase "signature" <| fun () ->
        let f (mid : (Message -> Message) -> Message -> Message) =
          ()
        f (Middleware.host "local")

      (*
        Skip: can't make FsCheck behave...
      testPropertyWithConfig fsCheckConfig "identity" <| fun (m : Message) ->
        let rm = ref (event Fatal "nope")
        let save m = rm := m; m
        let out = Middleware.identity save m
        Expect.equal out m "Identity property"
        Expect.equal (!rm) m "Middlware was called"*)
    ]

  ]

[<EntryPoint>]
let main args =
  let enUS = CultureInfo "en-US"
  Thread.CurrentThread.CurrentCulture   <- enUS
  Thread.CurrentThread.CurrentUICulture <- enUS
  Tests.runTestsInAssembly defaultConfig args