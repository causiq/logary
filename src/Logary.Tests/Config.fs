module Logary.Tests.Config

open Fuchu
open Swensen.Unquote
open TestDSL
open Fac

open Logary
open Logary.Configuration
open Logary.Targets

type Assert =
  static member Contains(msg : string, xExpected : 'a, xs : 'a seq) =
    match Seq.tryFind ((=) xExpected) xs with
    | None -> Tests.failtestf "%s -- expected %A to contain %A" msg xs xExpected
    | Some _ -> ()

[<Tests>]
let tests =
  let throws f_conf f =
    try
      let conf = confLogary "tests" |> f_conf
      conf |> validate |> ignore
      Tests.failtest "expected validation failure for conf: %A" conf |> ignore
    with :? ValidationException as e ->
      f e

  let r1 = Rule.forAny "not-correct-target"
  let t1 = Noop.create Noop.empty "another-target-name"

  testList "invalid configs" [
    testCase "mismatched rules/targets" <| fun _ ->
      throws
        (withRule r1 >> withTarget t1)
        (fun ex ->
          Assert.Contains("should contain orphan rule", r1, ex.InvalidRules)
          Assert.Contains("should contain orphan target", t1, ex.InvalidTargets)
          Assert.Equal("should have zero invalid metrics", [], ex.InvalidMetrics))

    ]