module Logary.Tests.Metrics

open Fuchu
open NodaTime
open Hopac
open Logary
open Logary.Metrics
open Logary.Metrics.Reservoirs

module Assert = ExpectoPatronum.Expect

[<Tests>]
let builtInMetrics =
  let take n stream =
    stream |> Stream.take n |> Stream.toSeq |> run |> Seq.toList

  testList "built-ins" [
    testList "simplest counter" [
      let counter = Counters.counter (PointName.ofSingle "logins") |> run

      yield testCase "initial" <| fun _ ->
        let stream = Metric.tap counter
        Metric.tick counter |> run
        Assert.equal (stream |> take 1L) [Int64 0L, Units.Scalar] "zero at start"

      yield testCase "counting to three" <| fun _ ->
        let stream = Metric.tap counter
        counter |> Metric.update (Int64 1L, Units.Scalar) |> run
        Metric.tick counter |> run
        counter |> Metric.update (Int64 1L, Units.Scalar) |> run
        counter |> Metric.update (Int64 1L, Units.Scalar) |> run
        Metric.tick counter |> run
        let actual = stream |> take 2L
        Assert.equal actual [
            Int64 1L, Units.Scalar
            Int64 2L, Units.Scalar
          ] "one and then two after two single counts"
    ]
  ]

[<Tests>]
let snapshot =
  let sample = Snapshot.create [| 5L; 1L; 2L; 3L; 4L |]
  let empty = Snapshot.create [||]

  testList "calculating snapshot values" [
    testCase "small quantiles are first value" <| fun _ ->
      Assert.floatEqual (Snapshot.quantile sample 0.) 1. None "should be the one"

    testCase "big quantiles are last values" <| fun _ ->
      Assert.floatEqual (Snapshot.quantile sample 1.) 5. None "should be the five"

    testCase "median" <| fun _ ->
      Assert.floatEqual (Snapshot.median sample) 3. None "should have median"

    testCase "75th percentile" <| fun _ ->
      Assert.floatEqual (Snapshot.percentile75th sample) 4.5 None "should have 75th percentile"

    testCase "95th percentile" <| fun _ ->
      Assert.floatEqual (Snapshot.percentile95th sample) 5. None "should have 95th percentile"

    testCase "98th percentile" <| fun _ ->
      Assert.floatEqual (Snapshot.percentile98th sample) 5. None "should have 98th percentile"

    testCase "99th percentile" <| fun _ ->
      Assert.floatEqual 5. (Snapshot.percentile99th sample) None "should have 99th percentile"

    testCase "999th percentile" <| fun _ ->
      Assert.floatEqual (Snapshot.percentile999th sample) 5. None "should have 999th percentile"

    testCase "has values" <| fun _ ->
      Assert.Equal("should have values ordered", Snapshot.values sample, [| 1L; 2L; 3L; 4L; 5L |])

    testCase "has size" <| fun _ ->
      Assert.Equal("should have five size", 5, Snapshot.size sample)

    testCase "has mimimum value" <| fun _ ->
      Assert.Equal("has a mimimum value", 1L, Snapshot.min sample)

    testCase "has maximum value" <| fun _ ->
      Assert.Equal("has a maximum value", 5L, Snapshot.max sample)

    testCase "has mean value" <| fun _ ->
      Assert.Equal("has a mean value", 3., Snapshot.mean sample)

    testCase "has stdDev" <| fun _ ->
      Assert.floatEqual (Snapshot.stdDev sample) 1.5811 (Some 0.0001) "has stdDev"

    testCase "empty: min" <| fun _ ->
      Assert.Equal("zero", 0L, Snapshot.min empty)

    testCase "empty: max" <| fun _ ->
      Assert.Equal("zero", 0L, Snapshot.max empty)

    testCase "empty: mean" <| fun _ ->
      Assert.floatEqual (Snapshot.mean empty) 0. None "zero"

    testCase "empty: std dev" <| fun _ ->
      Assert.floatEqual (Snapshot.mean empty) 0. None "zero"
    ]

[<Tests>]
let reservoirs =
  testList "reservoirs" [
    testCase "uniform: update 1000 times" <| fun _ ->
      let state =
        [ 0L .. 999L ]
        |> List.fold Uniform.update (Uniform.create 100)
      Assert.Equal("should have 100L size", 100, Uniform.size state)

      let snap = Uniform.snapshot state
      Assert.Equal("snapshot has as many", 100, Snapshot.size snap)
      for v in snap.values do
        Assert.Equal(sprintf "'%d' should be in [0, 999]" v, true, 0L <= v && v <= 999L)

    testCase "sliding: small" <| fun _ ->
      let state =
        [ 1L; 2L ]
        |> List.fold SlidingWindow.update (SlidingWindow.create 3)
      let snap = SlidingWindow.snapshot state
      Assert.Equal("has two", 2I, state.count)
      Assert.Equal("size has two", 2, SlidingWindow.size state)
      Assert.Equal("snap has two", 2, snap.values.Length)
      Assert.Equal("should have correct order", [| 1L; 2L; |], Snapshot.values snap)

    testCase "sliding: only last values" <| fun _ ->
      let state =
        [ 1L..5L ]
        |> List.fold SlidingWindow.update (SlidingWindow.create 3)
      let snap = SlidingWindow.snapshot state
      Assert.Equal("should have correct order", [| 3L..5L |], Snapshot.values snap)

    testList "sliding time window" [
      testCase "store duplicate ticks" <| fun _ ->
        let testClock =
          { new IClock with
              member x.Now = Instant(20L) }
        // TODO: might port sliding time window reservoir
        ()
      ]

    testList "exponentially weighted moving average" [
      let testEWMA explaination instance (expectations : _ list) =

        let flip f a b = f b a
        let passMinute s = // 5 second sampling rate, see implementation module
          [ 1..12 ] |> List.fold (fun s' t -> ExpWeightedMovAvg.tick s') s

        let initState =
          instance |> (flip ExpWeightedMovAvg.update) 3L |> ExpWeightedMovAvg.tick

        let actual =
          [ for i in 1..expectations.Length - 1 do yield i ]
          |> List.scan (fun s t -> passMinute s) initState
          |> List.map (ExpWeightedMovAvg.rate (Duration.FromSeconds 1L))

        testCase explaination <| fun _ ->
          List.zip expectations actual
          |> List.iteri (fun index (expected, actual) ->
            Assert.floatEqual actual expected (Some 0.00000001)
                              (sprintf "Index %d, should calculate correct EWMA" index))

      yield testEWMA "1 min"
        ExpWeightedMovAvg.oneMinuteEWMA
        [ 0.6
          0.22072766
          0.08120117
          0.02987224
          0.01098938
          0.00404277
          0.00148725
          0.00054713
          0.00020128
          0.00007405
          0.00002724
          0.00001002
          0.00000369
          0.00000136
          0.00000050
          0.00000018 ]

      yield testEWMA "5 min"
        ExpWeightedMovAvg.fiveMinutesEWMA
        [ 0.6
          0.49123845
          0.40219203
          0.32928698
          0.26959738
          0.22072766
          0.18071653
          0.14795818
          0.12113791
          0.09917933
          0.08120117
          0.06648190
          0.05443077
          0.04456415
          0.03648604
          0.02987224 ]

      yield testEWMA "15 min"
        ExpWeightedMovAvg.fifteenMinuteEWMA
        [ 0.6
          0.56130419
          0.52510399
          0.49123845
          0.45955700
          0.42991879
          0.40219203
          0.37625345
          0.35198773
          0.32928698
          0.30805027
          0.28818318
          0.26959738
          0.25221023
          0.23594443
          0.22072766 ]
      ]
    ]