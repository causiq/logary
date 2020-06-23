module Logary.Tests.DateTime

open System
open Expecto
open Expecto.Flip
open NodaTime
open FsCheck
open Logary
open Logary.Tests

[<Tests>]
let tests =
  testList "dates and time" [
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
          epochNanoS = dto.asTimestamp)
    ]

    testCase "DateTimeOffset.ofEpoch" <| fun _ ->
      let ts = SystemClock.Instance.GetCurrentInstant()
      let tsns = ts.ToUnixTimeTicks() * Constants.NanosPerTick
      let subject = DateTimeOffset.ofEpoch tsns
      subject.Year |> Expect.equal "Year eq" (ts.ToDateTimeOffset().Year)
      subject.Second |> Expect.equal "Second eq" (ts.ToDateTimeOffset().Second)
      subject.Millisecond |> Expect.equal "Millisecond eq" (ts.ToDateTimeOffset().Millisecond)

  ] |> testLabel "logary"