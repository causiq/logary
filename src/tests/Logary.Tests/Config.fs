module Logary.Tests.Config

open Fuchu
open Swensen.Unquote
open TestDSL
open Fac

open NodaTime

open Logary
open Logary.Configuration
open Logary.Targets

type Assert =
  static member Contains(msg : string, xExpected : 'a, xs : 'a seq) =
    match Seq.tryFind ((=) xExpected) xs with
    | None -> Tests.failtestf "%s -- expected %A to contain %A" msg xs xExpected
    | Some _ -> ()

[<Tests>]
let ``invalid configs`` =
  let throws f_conf f =
    try
      let conf = confLogary "tests" |> f_conf
      conf |> validate |> ignore
      Tests.failtest "expected validation failure for conf: %A" conf |> ignore
    with :? ValidationException as e ->
      f e

  let r1 = Rule.createForTarget (PointName.ofSingle "r1")
  let t1 = Noop.create Noop.empty (PointName.ofSingle "r1")
  let m1 = Metrics.Noop.create Metrics.Noop.empty (PointName.ofSingle "r1") (Duration.FromMilliseconds 500L)

  testList "invalid configs" [
    testCase "mismatched rules/targets" <| fun _ ->
      throws
        (withRule r1 >> withTarget t1)
        (fun ex ->
          Assert.Contains("should contain orphan rule", r1, ex.InvalidRules)
          Assert.Contains("should contain orphan target", t1, ex.InvalidTargets)
          Assert.Equal("should have zero invalid metrics", Set.empty, ex.InvalidMetrics))

    testCase "missing target" <| fun _ ->
      throws (withRule r1) (fun ex ->
          Assert.Contains("should contain orphan rule", r1, ex.InvalidRules)
          Assert.StringContains("string should contain name of rule", "r1", sprintf "%O" ex))

    testCase "missing rule" <| fun _ ->
      throws (withTarget t1) (fun ex ->
          Assert.Contains("should contain orphan target", t1, ex.InvalidTargets)
          Assert.StringContains("string should contain name of target", "t1", sprintf "%O" ex))

    testCase "missing rule for metric" <| fun _ ->
      throws (withMetric m1) (fun ex ->
          Assert.Contains("should contain orphan metric", m1, ex.InvalidMetrics)
          Assert.StringContains("string should contain name of metric", "m1", sprintf "%O" ex))
    ]

[<Tests>]
let ``valid configs`` =
  testList "valid configs" [
    testCase "rule for metric, no noop target" <| fun _ ->
      ()
    ]