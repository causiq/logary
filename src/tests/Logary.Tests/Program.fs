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
      testList "scaling" [
        testList "s" [
          testCase "0.0000001 s" <| fun _ ->
            Expect.equal (Units.scale Seconds 0.0000001) (100., "ns")
                        "Should be scaled to 100 ns"

          testCase "0.0001 s" <| fun _ ->
            Expect.equal (Units.scale Seconds 0.0001) (100., "µs")
                        "Should be scaled to 100 µs"

          testCase "0.1 s" <| fun _ ->
            Expect.equal (Units.scale Seconds 0.1) (100., "ms")
                        "Should be scaled to 100 ms"

          testCase "1 s" <| fun _ ->
            Expect.equal (Units.scale Seconds 1.) (1., "s")
                        "Should be scaled to 1 s"

          testCase "10 s" <| fun _ ->
            Expect.equal (Units.scale Seconds 10.) (10., "s")
                        "Should be scaled to 10 s"

          testCase "60 s" <| fun _ ->
            Expect.equal (Units.scale Seconds 60.) (1., "min")
                        "Should be scaled to 1 min"

          testCase "100 s" <| fun _ ->
            let value, units = Units.scale Seconds 100.
            Expect.floatClose Accuracy.veryHigh value 1.666666667
                              "Should be scaled to 1 2/3 min"
            Expect.equal units "min" "Should be scaled to minutes."
        ]

        testList "bits" [
          testCase "1 bit" <| fun _ ->
            Expect.equal (Units.scale Bits 1.) (1., "bit")
                        "Should be passed through (1 bit)"

          testCase "100 bit" <| fun _ ->
            Expect.equal (Units.scale Bits 100.) (100., "bit")
                        "Should be passed through (100 bit)"

          testCase "10000 bit" <| fun _ ->
            Expect.equal (Units.scale Bits 10000.) (10., "kbit")
                        "Should be scaled to 10 kbit"

          testCase "1 000 000 bit" <| fun _ ->
            Expect.equal (Units.scale Bits 1000000.) (1., "Mbit")
                        "Should be scaled to 1 Mbit"

          testCase "10 000 000 bit" <| fun _ ->
            Expect.equal (Units.scale Bits 10000000.) (10., "Mbit")
                        "Should be scaled to 10 Mbit"

          testCase "1 000 000 000 bit" <| fun _ ->
            Expect.equal (Units.scale Bits 1000000000.) (1., "Gbit")
                        "Should be scaled to 10 Gbit"

          testCase "2 500 000 000 000 bit" <| fun _ ->
            Expect.equal (Units.scale Bits 2500000000000.) (2.5, "Tbit")
                        "Should be scaled to 2.5 Tbit"

          testCase "2 000 000 000 000 000 bit" <| fun _ ->
            Expect.equal (Units.scale Bits 2000000000000000.) (2., "Pbit")
                        "Should be scaled to 2. Pbit"

          testCase "2 000 000 000 000 000 000 bit" <| fun _ ->
            let value, units = Units.scale Bits 2000000000000000000.
            Expect.floatEqual value 2. None "Should be scaled to 2. Ebit"
            Expect.equal units "Ebit" "Should be scaled to 2. Ebit"
        ]

        testList "bytes" [
          testCase "1 byte" <| fun _ ->
            Expect.equal (Units.scale Bytes 1.) (1., "B") "Should scale 1<->1 bytes"

          testCase "10 bytes" <| fun _ ->
            Expect.equal (Units.scale Bytes 10.) (10., "B") "Should not scale"

          testCase "1024 bytes" <| fun _ ->
            Expect.equal (Units.scale Bytes 1024.) (1., "KiB") "Should scale to KiB"

          testCase "(2^10)^2 bytes" <| fun _ ->
            Expect.equal (Units.scale Bytes (1024. * 1024.)) (1., "MiB") "Should scale to MiB"

          testCase "(2^10)^3 bytes" <| fun _ ->
            Expect.equal (Units.scale Bytes (1024. * 1024. * 1024.)) (1., "GiB")
                        "Should scale to GiB"

          testCase "(2^10)^4 bytes" <| fun _ ->
            Expect.equal (Units.scale Bytes (1024. * 1024. * 1024. * 1024.)) (1., "TiB")
                        "Should scale to TiB"
        ]

        testCase "scalars are not scaled" <| fun _ ->
          Expect.equal (Units.scale Scalar 12345678.) (12.345678, "M")
                      "Should not present a unit for Scalars"

        testCase "others are not scaled" <| fun _ ->
          Expect.equal (Units.scale (Other "reqs") 12345678.) (12345678., "reqs")
                      "Should not present a unit for Scalars"

        testCase "Percent are always scalled x100 and presented with a % symbol" <| fun _ ->
          Expect.equal (Units.scale Percent 0.1246) (12.46, "%")
                      "Percents are scaled properly"

        testCase "'Scaled' unit with a 1. scale is not actually scaled" <| fun _ ->
          let actual = Units.scale (Scaled (Percent, 1.)) 0.123
          let expected = 12.3, "%"
          Expect.equal actual expected "Should handle non-scaled Scaled"

        testCase "'Scaled' unit by 1/10" <| fun _ ->
          let actual = Units.scale (Scaled (Percent, 0.1)) 0.123
          // if 12.3% is had been scaled by 0.1, then the true value is this:
          let expected = 123., "%"
          Expect.equal actual expected "Should handle 0.1x scale"

        testList "SI thousands-units multiples" [
          yield!
            [ 1., 1., ""
              10., 10., ""
              100., 100., ""
              1000., 1., "k"
              1024., 1.024, "k"
              2345., 2.345, "k"
              1000000., 1., "M"
              1234000333., 1.234000333, "G"
              1234000333444., 1.234000333444, "T"
              1234000333444555., 1.234000333444555, "P"
              1234000333444555666., 1.234000333444555666, "E"
              1234000333444555666777., 1.234000333444555666777, "Z"
            ]
            |> List.collect (fun (value, expectedf, prefix) ->
            [ Metres; Amperes; Kelvins; Moles; Candelas; Watts; Hertz ] |> List.map (fun units ->
            testCase (sprintf "scaling %f %A" value units) (fun _ -> 
              let actualf, actualu = Units.scale units value
              let expectedu = sprintf "%s%s" prefix (Units.symbol units)
              Expect.equal actualu expectedu "Should properly format the unit"
              Expect.floatEqual actualf expectedf None "Should properly scale the value to the unit"
            )))
        ]

        testCase "scaleBy10 Seconds 0.0001" <| fun _ ->
          let actualf, actualu = Units.scaleBy10 Seconds 0.0001
          let expectedf, expectedu = 1e6, "μs"
          Expect.floatEqual actualf expectedf None "Should scale 0.0001 properly"
          Expect.equal actualu expectedu "Should scale 0.0001's unit properly"

        testCase "scaleBy10 Seconds 0.00011" <| fun _ ->
          let actualf, actualu = Units.scaleBy10 Seconds 0.00011
          let expectedf, expectedu = 1e6, "μs"
          Expect.floatEqual actualf expectedf None "Should scale 0.00011 properly"
          Expect.equal actualu expectedu "Should scale 0.00011's unit properly"

        testList "SI thousands-units fractions" [
          yield!
            [ 1., 1., ""
              0., 0., ""
              0.1, 100., "m"
              0.01, 10., "m"
              0.002, 2., "m"
              0.001, 1., "m"
              0.001024, 1.024, "m"
              0.0009, 900., "μ"
              0.000222, 222., "μ"
              0.000000033, 33., "n"
              0.000000000404, 404., "p"
              2.345e-15, 2.345, "f"
              34.221e-18, 34.221, "a"
              821.14e-21, 821.14, "z"
              3.1e-24, 3.1, "y"
            ]
            |> List.collect (fun (value, expectedf, prefix) ->
            [ Metres; Amperes; Kelvins; Moles; Candelas; Watts; Hertz ] |> List.map (fun units ->
            testCase (sprintf "scaling %A %A" value units) (fun _ ->
              let actualf, actualu = Units.scale units value
              let expectedu = sprintf "%s%s" prefix (Units.symbol units)
              Expect.equal actualu expectedu "Should properly format the unit"
              Expect.floatEqual actualf expectedf None "Should properly scale the value to the unit"
            )))
        ]
      ]
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