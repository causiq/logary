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

[<Tests>]
let tests =
  testList "logary" [
    testList "FsCheck" [
      testPropertyWithConfig fsc "NormalFloat" <| fun (NormalFloat f) ->
        Expect.isNotNaN f "Should be a number"
        Expect.isNotInfinity f "Should be a real number"
        Expect.isNotPositiveInfinity f "Should be a real number"
        Expect.isNotNegativeInfinity f "Should be a real number"
        true
    ]

    testList "core" [
      testList "HashMap" HashMap.tests
      testList "LogLevel" LogLevel.tests
      testList "Constants" Constants.tests
      testList "Global" Global.tests
      testList "Value" Value.tests
      testList "Units" Units.tests
      testList "PointName" PointName.tests
      testList "Message" Message.tests
    ]

    testList "configuration" [
      testList "Config" Config.tests
      testList "Transformers" Transformers.tests
    ]

    testList "LoggerScope" [
      testCase "public interface :> Logger" <| fun () ->
        let hasLogger = typeof<Logger>.IsAssignableFrom(typeof<LoggerScope>)
        Expect.isTrue hasLogger "Should implement Logger"

      testCase "public interface :> IDisposable" <| fun () ->
        let hasIDisposable = typeof<IDisposable>.IsAssignableFrom(typeof<LoggerScope>)
        Expect.isTrue hasIDisposable "Should implement IDisposable"
    ]

    testList "TimeLogger" [
      testCase "public interface" <| fun () ->
        { new TimeLogger with
            member x.name: PointName = PointName.ofSingle "B"
            member x.logWithAck (waitForBuffers, level) (messageFactory: LogLevel -> Message) =
              LogResult.success
            member x.level: LogLevel = LogLevel.Info
            member x.Dispose () = ()
            member x.elapsed = Duration.Zero
            member x.bisect (label: string): unit =
              ()
            member x.finish (transform: Message -> Message) = ()
        }
        |> ignore
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

      testPropertyWithConfig fsc "identity" <| fun (m: Message) ->
        let rm = ref (event Fatal "nope")
        let save m = rm := m; m
        let out = Middleware.identity save m
        Expect.equal out m "Identity property"
        Expect.equal (!rm) m "Middlware was called"
    ]

    testList "NodaTime.Duration" [
      testPropertyWithConfig fsc "toGauge()" <| fun (d: Duration) ->
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
    testList "Units" Units.tests

    testList "core targets" CoreTargets.tests
    testLabel "literate console" LiterateConsole.tokenisation
    testLabel "literate console" LiterateConsole.parts

    testLabel "formatting" (Json.tests fsc)
    testLabel "formatting" Formatting.MessageWriters.tests
    testLabel "formatting" (Formatting.MessageWriters.stacktrace fsc)

    Codecs.tests

    testList "Registry" Tests.Registry.tests
    testList "Metric" Logary.Tests.Metrics.tests
  ]

[<EntryPoint>]
let main args =
  let enUS = CultureInfo "en-US"
  Thread.CurrentThread.CurrentCulture   <- enUS
  Thread.CurrentThread.CurrentUICulture <- enUS
  Tests.runTestsInAssembly defaultConfig args