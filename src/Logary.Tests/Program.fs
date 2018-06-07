module Logary.Tests.Program

#nowarn "44"

open System
open System.Globalization
open System.Threading
open Hopac
open NodaTime
open Expecto
open Logary
open Logary.Internals
open Logary.Message
open Logary.Configuration
open FsCheck

type SampleObject() =
  member x.PropA =
    45
  member x.PropB =
    raise (Exception ("Oh noes, no referential transparency here"))

type Domain =
  Domain of hiera:string[]
with
  member x.value =
    let (Domain values) = x
    String.concat "." values

type Arbs =
  static member Domain (): Arbitrary<Domain> =
    gen {
      let! top = ["se"; "com"; "net"] |> Gen.elements
      let! sub = [ "haf"; "qvitoo"; "bücher.ch"] |> Gen.elements
      return Domain [| sub; top |]
    }
    |> Arb.fromGen

  static member Uri (): Arbitrary<Uri> =
    let legalChars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-._~:#[]@!$&'()*+,;=".ToCharArray()
    let segment = gen {
      let! l = Gen.choose(1,15)
      let! chars = Gen.arrayOfLength l (Gen.elements legalChars)
      return string chars
    }
    gen {
      let! scheme = ["http"; "https"] |> Gen.elements
      let! domain = Arbs.Domain () |> Arb.toGen
      let! segs = Gen.choose(1,6)
      let! segments = Gen.listOfLength segs segment
      let path = "/" + (String.concat "/" segments)
      return Uri (sprintf "%s://%s%s" scheme domain.value path)
    }
    |> Arb.fromGen

  static member Duration() =
    Arb.Default.TimeSpan()
    |> Arb.convert (Duration.FromTimeSpan) (fun d -> d.ToTimeSpan())

  static member HashMap() =
    let nonNullKey = fun (KeyValue (k, _)) -> not (isNull (box k))
    let filter list = List.filter nonNullKey list
    Arb.Default.FsList()
    |> Arb.convert (filter >> HashMap.ofListPair) HashMap.toListPair

  static member Value() =
    let floats = Arb.generate<NormalFloat> |> Gen.map (fun (NormalFloat f) -> Float f)
    let int64s = Arb.from<int64> |> Arb.convert Int64 (function Int64 ii -> ii | _ -> failwith "Not an Int64")
    let bigints = Arb.from<bigint> |> Arb.convert BigInt (function BigInt bi -> bi | _ -> failwith "Not a BigInt")
    let generator =
      Gen.frequency [
        6, floats
        6, int64s.Generator
        2, bigints.Generator
      ]
    let shrinker = function
      | Float f -> Arb.shrink f |> Seq.map Float
      | Int64 _ as ii -> int64s.Shrinker ii
      | BigInt _ as bi -> bigints.Shrinker bi
      | otherwise -> Seq.empty
    Arb.fromGenShrink (generator, shrinker)

  static member Units() =
    let isNormal f =
         not <| Double.IsInfinity f
      && not <| Double.IsNaN f
    Arb.Default.Derive()
    |> Arb.filter (function
      | Pow (_, n)    -> isNormal n
      | Offset (_, f) -> isNormal f
      | Scaled (_, f) -> isNormal f
      | _ -> true)

  static member Gauge() =
    let isNormal f =
         not <| Double.IsInfinity f
      && not <| Double.IsNaN f
    Arb.Default.Derive()
    |> Arb.filter (function | Gauge (f, units) -> isNormal (f.toFloat()))

  static member Instant() =
    Arb.Default.DateTimeOffset()
    |> Arb.convert Instant.FromDateTimeOffset (fun i -> i.ToDateTimeOffset())

  static member Exception() =
    let failer message =
      failwith message
    let meth message =
      failer message
    let another message =
      meth message
    let generator =
      gen {
        let! (NonEmptyString message) = Arb.generate<NonEmptyString>
        let! (NonEmptyString message2) = Arb.generate<NonEmptyString>
        let! hasInner =
          Gen.frequency [
            1, Gen.constant false
            2, Gen.constant true
          ]
        return
          try another message
          with e ->
            e
      }

    let shrinker (e: exn): seq<exn> =
      if not (isNull e.InnerException) then Seq.singleton e.InnerException
      else Seq.empty

    Arb.fromGenShrink (generator, shrinker)

let fsCheckConfig =
  { FsCheckConfig.defaultConfig with arbitrary = [ typeof<Arbs> ] }

[<Tests>]
let tests =
  testList "logary" [
    testList "FsCheck" [
      testPropertyWithConfig fsCheckConfig "NormalFloat" <| fun (NormalFloat f) ->
        Expect.isNotNaN f "Should be a number"
        Expect.isNotInfinity f "Should be a real number"
        Expect.isNotPositiveInfinity f "Should be a real number"
        Expect.isNotNegativeInfinity f "Should be a real number"
        true
    ]

    testList "HashMap" [
      testCase "empty and add element" <| fun () ->
        HashMap.empty |> HashMap.add 2 4 |> ignore
    ]

    testList "LogLevel" [
      testProperty "toInt" <| fun (level: LogLevel) ->
        level.toInt() |> ignore

      testProperty "=" <| fun (level: LogLevel) ->
        level = level

      testProperty "<>" <| fun (level: LogLevel) ->
        if level = Info then level = Info else
        level <> Info
    ]

    testList "KnownLiterals" (
      [
        KnownLiterals.LogaryPrefix, "_logary."
        KnownLiterals.FieldsPrefix, "_fields."
        KnownLiterals.GaugeNamePrefix, "_logary.gauge."

        KnownLiterals.ErrorsContextName, "_logary.errors"
        KnownLiterals.ServiceContextName, "_logary.service"
        KnownLiterals.HostContextName, "_logary.host"
        KnownLiterals.TagsContextName, "_logary.tags"

        KnownLiterals.DefaultGaugeName, "default-gauge"
      ]
      |> List.map (fun (actual, expected) ->
          testCase (sprintf "ensuring constant '%s'" expected)<| fun () ->
            Expect.equal actual expected "KnownLiteral should not change"
      )
    )

    testList "Constants (floats)" [
      testList "floats" (
        [ Constants.SecondsPerTick, 0.0000001, "SecondsPerTick"
          Constants.MillisPerTick, 0.0001, "MillisPerTick"
          Constants.MicrosPerTick, 0.1, "MicrosPerTick"
        ]
        |> List.map (fun (actual, expected, name) ->
            testCase (sprintf "ensuring constant '%s'" name) <| fun () ->
              Expect.equal actual expected "Constant should not change"
        )
      )

      testList "int64s" (
        [ Constants.NanosPerTick, 100L, "NanosPerTick"
          Constants.NanosPerMicro, 1000L, "NanosPerMicro"
          Constants.NanosPerMilli, 1000000L, "NanosPerMilli"
          Constants.NanosPerSecond, 1000000000L, "NanosPerSecond"
          Constants.NanosPerMinute, 60000000000L, "NanosPerMinute"
          Constants.TicksPerMinute, 600000000L, "TicksPerMinute"
          Constants.TicksPerSecond, 10000000L, "TicksPerSecond"
          Constants.TicksPerMilli, 10000L, "TicksPerMilli"
          Constants.TicksPerMicro, 10L, "TicksPerMicro"
        ]
        |> List.map (fun (actual, expected, name) ->
        testCase (sprintf "ensuring constant '%s'" name) <| fun () ->
          Expect.equal actual expected "Constant should not change"
        )
      )
    ]

    testList "Config" [
      testCase "create" <| fun () ->
        Config.create "tests" "hostname-123" |> ignore
    ]

    testList "Value" [
      testCase "Float" <| fun () -> ignore (Float 3.)
      testCase "Int64" <| fun () -> ignore (Int64 3L)
      testCase "BigInt" <| fun () -> ignore (BigInt 3I)
      testPropertyWithConfig fsCheckConfig "Value" <| fun (value: Value) ->
        match value with
        | Float f ->
          Expect.isNotNaN f "Should be a number"
          Expect.isNotInfinity f "Should be a real number"
          Expect.isNotPositiveInfinity f "Should be a real number"
          Expect.isNotNegativeInfinity f "Should be a real number"
        | _ ->
          ()
        true
    ]

    testList "Units" [
      testPropertyWithConfig fsCheckConfig "Units" <| fun (u: Units) ->
        match u with
        | Offset (_, f) ->
          Expect.isNotNaN f "Should be a number"
          Expect.isNotInfinity f "Should not be infinity"
          true
        | _ ->
          true

      testPropertyWithConfig fsCheckConfig "symbol can be called" <| fun (u: Units) ->
        try
          u.symbol |> ignore
          true
        with e ->
          Tests.failtestf "Should not throw, but did: %O" e
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

      testPropertyWithConfig fsCheckConfig "isEmpty is callable for all" <| fun (pn: PointName) ->
        pn.isEmpty |> ignore
        true

      testPropertyWithConfig fsCheckConfig "ToString is callable for all" <| fun (pn: PointName) ->
        pn.ToString() |> ignore
        true
    ]

    testList "HashMap" [
      testCase "add" <| fun _ ->
        HashMap.empty |> HashMap.add "a" 24 |> ignore

      testCase "add + remove" <| fun _ ->
        let subject = HashMap.empty |> HashMap.add "a" 24 |> HashMap.remove "a"
        Expect.isFalse (subject |> HashMap.containsKey "a") "Should not contain 'a'"

      testCase "add x2" <| fun _ ->
        let subject = HashMap.empty |> HashMap.add "a" 2 |> HashMap.add "b" 3
        Expect.isTrue (subject |> HashMap.containsKey "a") "Should have 'a' key"
        Expect.isTrue (subject |> HashMap.containsKey "b") "Should have 'b' key"
        let subject = subject |> HashMap.remove "b"
        Expect.isTrue (subject |> HashMap.containsKey "a") "Should have 'a' key"
        Expect.isFalse (subject |> HashMap.containsKey "b") "Should have 'b' key"

      testPropertyWithConfig fsCheckConfig "generate" <| fun (value: HashMap<string, int>) ->
        ignore value
        true
    ]

    testList "Message" [
      testCase "event: LogLevel -> string -> Message" <| fun _ ->
        let m = event Info "Hello world"
        Expect.equal m.level Info "Should have info level"
        Expect.equal m.value ("Hello world") "Should have template"

      testCase "eventX: string -> LogLevel -> Message" <| fun _ ->
        let m = eventX "Hello world" Info
        Expect.equal m.level Info "Should have info level"
        Expect.equal m.value ("Hello world") "Should have template"

      testProperty "tryGetContext after setContext" <| fun name ->
        eventInfo "" |> setContext name name |> tryGetContext name
        |> function | v when isNull name -> Option.isNone v | v -> v = (Some name)

      testCase "setContext override value when name exist" <| fun _ ->
        let get = tryGetContext "key" >> Option.get
        let set v = setContext "key" v
        let m1 = eventInfo "" |> set 1
        Expect.equal (get m1) 1 "Should properly set context value"
        let m2 = m1 |> set 2
        Expect.equal (get m2) 2 "Should override"

      testProperty "setField should have field prefix" <| fun name ->
        let prefixName = KnownLiterals.FieldsPrefix + name
        let fv = eventInfo "" |> setField name 1 |> tryGetContext prefixName
        Expect.isSome fv "Value should be found"
        Expect.equal fv.Value 1 "Value should be found"

      testProperty "tryGetField" <| fun name ->
        let msg = eventInfo ""
        let value = msg |> tryGetField name
        Expect.isNone value "Should not be found"
        let value = msg |> setField name name |> tryGetField name
        Expect.equal value (Some name) "Should be same"

      testCase "getAllFields" <| fun _ ->
        let names = Arb.generate<NonEmptyString> |> Gen.sample 0 5 |> List.distinct |> List.map (fun (NonEmptyString name) -> name)
        names
        |> List.fold (fun m name ->
           m |> setField name name) (eventInfo "")
        |> getAllFields |> Seq.length |> fun c ->
           Expect.equal c names.Length "Should get same length after set fields"

      testPropertyWithConfig fsCheckConfig "gaugeMessage" <| fun g ->
        let saved =
          gaugeWithUnit (PointName.parse "Car.Speedometer") "dv/dt" g
          |> tryGetGauge "dv/dt"
        Expect.equal saved (Some g) "Should be same"

      testCase "getAllGauges" <| fun _ ->
        let names = Arb.generate<NonEmptyString> |> Gen.sample 0 5 |> List.distinct |> List.map (fun (NonEmptyString name) -> name)
        let msg = names |> List.fold (fun m name -> m |> addGauge name (Gauge (Float 1., Units.Scalar))) (eventInfo "")
        let c = msg |> getAllGauges |> Seq.length
        Expect.equal c names.Length "Should get same length after add gauges"

      testCase "getAllTags & hasTag" <| fun _ ->
        let tags = Arb.generate<NonEmptyString> |> Gen.sample 0 5 |> List.distinct |> List.map (fun (NonEmptyString name) -> name)
        let msg = tags |> List.fold (fun m name -> m |> tag name) (eventInfo "")
        msg |> getAllTags |> fun tagSet ->
           Expect.equal tagSet.Count tags.Length "Should get same length after add tags"
           Expect.equal (tags |> Set.ofList) tagSet "Should be same set"
           Expect.containsAll tagSet tags "Should contains all tags"
        let msgHasAllTag = tags |> List.forall (fun tag -> hasTag tag msg )
        Expect.isTrue msgHasAllTag "Should have all tags"

      testCase "setFieldsFromObject: obj -> Message -> Message" <| fun () ->
        let m = eventX "Hello world" Info |> setFieldsFromObject (SampleObject())
        let field = m |> tryGetField "PropA"
        Expect.equal field (Some 45) "Should have PropA"

      testCase "eventFormat" <| fun _ ->
        let m = Message.eventFormat (Info, "some {@data} created at {$time}", 1,2)
        let dataf = m |> tryGetField "data"
        let timef = m |> tryGetField "time"
        Expect.equal dataf (Some 1) "Should have data field"
        Expect.equal timef (Some 2) "Should have time field"

      testCase "addExn & getExns" <| fun _ ->
        let e1 = Exception ("e1")
        let e2 = ArgumentNullException ("e2")
        let errors = eventInfo "" |> addExn e1 |> addExn e2 |> getExns
        Expect.contains errors e1 "Should have exn"
        Expect.contains errors (upcast e2) "Should have arg null exn"

      testCase "time and timeJob" <| fun _ ->
        let name = PointName.parse "some.gauge.at.location"
        let timeFun = time name id
        let res, msg = timeFun 100
        Expect.equal res 100 "Should have result"

        let g = tryGetGauge "time" msg
        Expect.isSome g "Should have guage"
        match g with
        | Some (Gauge (_, Units.Scaled (Seconds, _))) -> ()
        | g -> failtestf "Should have units.scaled (seconds, _) , actual: %A" g

      testCaseAsync "time & timeJob" <| (async {
        let name = "some.gauge.at.location"

        let test res msg =
          Expect.equal res 100 "Should have result"
          let g = tryGetGauge "time" msg
          Expect.isSome g "Should have guage"
          match g with
          | Some (Gauge (_, Units.Scaled (Seconds, _))) -> ()
          | g -> failtestf "Should have units.scaled (seconds, _) , actual: %A" g

        let timeFun = time (PointName.parse name) id
        let timeJobFun = timeJob (PointName.parse name) Job.result
        let! res1, msg1 = timeJobFun 100 |> Job.toAsync
        let res2, msg2 = timeFun 100
        test res1 msg1
        test res2 msg2
      })
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
            member x.name: PointName = PointName.ofSingle "B"
            member x.logWithAck (waitForBuffers, level) (messageFactory: LogLevel -> Message) =
              LogResult.success
            member x.level: LogLevel = LogLevel.Info
            member x.Dispose () = ()
            member x.elapsed = Duration.Zero
            member x.bisect (label: string): unit =
              ()
            member x.stop (decider: Duration -> LogLevel) =
              LogResult.success
        }
        |> ignore
    ]

    testList "UnitsModule" [
      testList "formatValue" (
        [ Float 62., "62"
          Int64 84598L, "84598"
          BigInt 1024I, "1024"
          Fraction (2L, 5L), "2/5"
        ]
        |> List.map (fun (value, expected) ->
            testCase (string value) (fun () ->
              let actual = Units.formatValue value
              Expect.equal actual expected "Should have correct result"
            )
        )
      )

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
            Expect.floatClose Accuracy.medium value 2. "Should be scaled to 2. Ebit"
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
              let expectedu = sprintf "%s%s" prefix units.symbol
              Expect.equal actualu expectedu "Should properly format the unit"
              Expect.floatClose Accuracy.veryHigh actualf expectedf "Should properly scale the value to the unit"
            )))
        ]

        testCase "scaleBy10 Seconds 0.0001" <| fun _ ->
          let actualf, actualu = Units.scaleBy10 Seconds 0.0001
          let expectedf, expectedu = 1e6, "μs"
          Expect.floatClose Accuracy.veryHigh actualf expectedf "Should scale 0.0001 properly"
          Expect.equal actualu expectedu "Should scale 0.0001's unit properly"

        testCase "scaleBy10 Seconds 0.00011" <| fun _ ->
          let actualf, actualu = Units.scaleBy10 Seconds 0.00011
          let expectedf, expectedu = 1e6, "μs"
          Expect.floatClose Accuracy.veryHigh actualf expectedf "Should scale 0.00011 properly"
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
              let expectedu = sprintf "%s%s" prefix units.symbol
              Expect.equal actualu expectedu "Should properly format the unit"
              Expect.floatClose Accuracy.veryHigh actualf expectedf "Should properly scale the value to the unit"
            )))
        ]
      ]
    ]

    testList "GaugeModule" [
      testCase "formatWithUnit" <| fun () ->
        let f = Gauge.format (Gauge (Float 2.34, Units.Days))
        Expect.equal f "2.34 days" "Should format # days properly"
    ]

    // TO CONSIDER: bring back Scheduling when needed

    testList "NullLogger" [
      testCase "should be named 'Logary.NullLogger'" <| fun () ->
        let sut = NullLogger.instance
        Expect.equal sut.name (PointName.parse "Logary.NullLogger")
                     "Is called Logary.NullLogger"

      testCaseJob "logWithAck success" (job {
        let sut = NullLogger.instance
        let! p = sut.logWithAck (true, Fatal) (eventX "hi")
        match p with
        | Ok ack ->
          do! ack
        | Result.Error e ->
          failtestf "%A" e
      })

      testCaseJob "logAck success" (job {
        do! NullLogger.instance.logAck Fatal (eventX "Hi")
      })

      testCaseJob "log success" (job {
        let sut = NullLogger.instance
        let! res = sut.log Fatal (eventX "hi")
        Expect.isTrue res "Should return true as a stubbed value"
      })
    ]

    testList "Middleware" [
      testCase "signature" <| fun () ->
        let f (mid : (Message -> Message) -> Message -> Message) =
          ()
        f (Middleware.host "local")

      testPropertyWithConfig fsCheckConfig "identity" <| fun (m: Message) ->
        let rm = ref (event Fatal "nope")
        let save m = rm := m; m
        let out = Middleware.identity save m
        Expect.equal out m "Identity property"
        Expect.equal (!rm) m "Middlware was called"
    ]

    testList "NodaTime.Duration" [
      testPropertyWithConfig fsCheckConfig "toGauge()" <| fun (d: Duration) ->
        ignore (d.toGauge())
    ]

    testList "NodaTime.Instant" [
      testProperty "ofEpoch" <| fun _ ->
        let nanoSeconds = Arb.from<int64> |> Arb.mapFilter ((*) 100L) ((>) 0L)
        Prop.forAll nanoSeconds (fun epochNanoS ->
          let instant = Instant.ofEpoch epochNanoS
          let dto = instant.ToDateTimeOffset ()
          epochNanoS = dto.timestamp)
    ]

    testCase "DateTimeOffset.ofEpoch" <| fun _ ->
      let ts = SystemClock.Instance.GetCurrentInstant()
      let tsns = ts.ToUnixTimeTicks() * Constants.NanosPerTick
      let subject = DateTimeOffset.ofEpoch tsns
      subject.Year |> Flip.Expect.equal "Year eq" (ts.ToDateTimeOffset().Year)
      subject.Second |> Flip.Expect.equal "Second eq" (ts.ToDateTimeOffset().Second)
      subject.Millisecond |> Flip.Expect.equal "Millisecond eq" (ts.ToDateTimeOffset().Millisecond)

    testList "Engine" Engine.tests

    testList "core targets" CoreTargets.tests
    testLabel "literate console" LiterateConsole.tests

    testLabel "Formatting" (Formatting.jsonTests fsCheckConfig)
    testLabel "Formatting" Formatting.textPrinters
    testLabel "Formatting" (Formatting.stacktrace fsCheckConfig)

    Codecs.tests

    testList "Registry" Tests.Registry.tests
  ]

[<EntryPoint>]
let main args =
  let enUS = CultureInfo "en-US"
  Thread.CurrentThread.CurrentCulture   <- enUS
  Thread.CurrentThread.CurrentUICulture <- enUS
  Tests.runTestsInAssembly defaultConfig args