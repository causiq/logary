module Logary.Tests.Config

open Fuchu
open Swensen.Unquote
open TestDSL
open Fac

open NodaTime

open Logary
open Logary.Configuration
open Logary.Targets
open Logary.Configuration_Uri

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
      Tests.failtestf "expected validation failure for conf: %A" conf |> ignore
    with :? ValidationException as e ->
      f e

  let r1 = Rule.createForTarget (PointName.ofSingle "r1")
  let t1 = Noop.create Noop.empty (PointName.ofSingle "t1")

  testList "invalid configs" [
    testCase "mismatched rules/targets" <| fun _ ->
      throws
        (withRule r1 >> withTarget t1)
        (fun ex ->
          Assert.Contains("should contain orphan rule", r1, ex.InvalidRules)
          Assert.Contains("should contain orphan target", t1, ex.InvalidTargets))

    testCase "missing target" <| fun _ ->
      throws (withRule r1) (fun ex ->
          Assert.Contains("should contain orphan rule", r1, ex.InvalidRules)
          Assert.StringContains("string should contain name of rule", "r1", sprintf "%O" ex))

    testCase "missing rule" <| fun _ ->
      throws (withTarget t1) (fun ex ->
          Assert.Contains("should contain orphan target", t1, ex.InvalidTargets)
          Assert.StringContains("string should contain name of target", "t1", sprintf "%O" ex))
    ]


type ArbConfig =
  { db : string
    batchSize : int }


[<Tests>]
let ``valid configs`` =
  testList "valid configs" [
    testCase "rule for metric, no noop target" <| fun _ ->
      ()

    testCase "parsing uri to target config" <| fun _ ->
      let expectedConfig = 
        { db = "databaseName"
          batchSize = 123 }

      let actualConfig = parseConfig<ArbConfig> "influxdb://user:pass@host:8086/write?db=databaseName&batchSize=123"

      Assert.equal actualConfig expectedConfig "Record should equal .....?"
      ()

    testCase "" <| fun _ ->
        let conn = "influxdb://root:root@host:8086/write?db=wadus"
        let test = "measurement,tkey1=tval1,tkey2=tval2 fkey=fval,fkey2=fval2 1234567890000000000"
        ()
    ]
