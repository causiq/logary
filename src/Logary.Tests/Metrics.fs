module Logary.Tests.Metrics

open Fuchu
open Logary
open Logary.Metrics

type Assert =
  static member FloatEqual(msg, expected, actual, ?epsilon) =
    let epsilon = defaultArg epsilon 0.001
    if expected <= actual + epsilon && expected >= actual - epsilon then
      ()
    else
      Tests.failtestf "Expected %f to be %f within %f epsilon. %s"
        actual expected epsilon msg

[<Tests>]
let snapshot =
  let sample = Snapshot.create [| 5L; 1L; 2L; 3L; 4L |]

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
      Assert.FloatEqual("has stdDev", 1.5811, Snapshot.stdDev sample)

    ]

[<Tests>]
let reservoirs =
  testList "uniform reservoirs" [
    testCase "update 100 times" <| fun _ ->
      let mutable state = UniformReservoir.create 100
      for i in 0L..999L do
        state <- UniformReservoir.update state i
      Assert.Equal("should have 100L size", 100, UniformReservoir.size state)
      let snap = UniformReservoir.snapshot state
      Assert.Equal("snapshot has as many", 100, Snapshot.size snap)
      for v in snap.values do
        Assert.Equal(sprintf "'%d' should be in [0, 999]" v, true, 0L <= v && v <= 999L)
    ]