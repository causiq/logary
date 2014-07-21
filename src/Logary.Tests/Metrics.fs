module Logary.Tests.Metrics

open Fuchu

open NodaTime

open Logary
open Logary.Metric.Reservoir

[<Tests>]
let snapshot =
  let sample = Snapshot.create [| 5L; 1L; 2L; 3L; 4L |]
  let empty = Snapshot.create [||]

  testList "calculating snapshot values" [
    testCase "small quantiles are first value" <| fun _ ->
      Assert.FloatEqual("should be the one", 1., Snapshot.quantile sample 0.)
    testCase "big quantiles are last values" <| fun _ ->
      Assert.FloatEqual("should be the five", 5., Snapshot.quantile sample 1.)
    testCase "median" <| fun _ ->
      Assert.FloatEqual("should have median", 3., Snapshot.median sample)
    testCase "75th percentile" <| fun _ ->
      Assert.FloatEqual("should have 75th percentile", 4.5, Snapshot.percentile75th sample)
    testCase "95th percentile" <| fun _ ->
      Assert.FloatEqual("should have 95th percentile", 5., Snapshot.percentile95th sample)
    testCase "98th percentile" <| fun _ ->
      Assert.FloatEqual("should have 98th percentile", 5., Snapshot.percentile98th sample)
    testCase "99th percentile" <| fun _ ->
      Assert.FloatEqual("should have 99th percentile", 5., Snapshot.percentile99th sample)
    testCase "999th percentile" <| fun _ ->
      Assert.FloatEqual("should have 999th percentile", 5., Snapshot.percentile999th sample)
    testCase "has values" <| fun _ ->
      Assert.Equal("should have values ordered", [| 1L; 2L; 3L; 4L; 5L |], Snapshot.values sample)
    testCase "has size" <| fun _ ->
      Assert.Equal("should have five size", 5, Snapshot.size sample)
    testCase "has mimimum value" <| fun _ ->
      Assert.Equal("has a mimimum value", 1L, Snapshot.min sample)
    testCase "has maximum value" <| fun _ ->
      Assert.Equal("has a maximum value", 5L, Snapshot.max sample)
    testCase "has mean value" <| fun _ ->
      Assert.Equal("has a mean value", 3., Snapshot.mean sample)
    testCase "has stdDev" <| fun _ ->
      Assert.FloatEqual("has stdDev", 1.5811, Snapshot.stdDev sample, 0.0001)
    testCase "empty: min" <| fun _ ->
      Assert.Equal("zero", 0L, Snapshot.min empty)
    testCase "empty: max" <| fun _ ->
      Assert.Equal("zero", 0L, Snapshot.max empty)
    testCase "empty: mean" <| fun _ ->
      Assert.FloatEqual("zero", 0., Snapshot.mean empty)
    testCase "empty: std dev" <| fun _ ->
      Assert.FloatEqual("zero", 0., Snapshot.mean empty)
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
          |> List.map (ExpWeightedMovAvg.rate Seconds)

        testCase explaination <| fun _ ->
          List.zip expectations actual
          |> List.iteri (fun index (expected, actual) ->
            Assert.FloatEqual(sprintf "Index %d, should calculate correct EWMA" index,
                              expected,  actual, epsilon = 0.00000001))

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