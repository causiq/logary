module Logary.Tests.Config

open System
open Fuchu
open Swensen.Unquote
open TestDSL
open Fac

open NodaTime

open Logary
open Logary.Configuration
open Logary.Configuration.Uri
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
    
type Consistency =
  | Yolo
  | Quorum

  /// Use this pattern for parsing DUs
  static member tryParse (s : string) =
    match s with
    | "Yolo" ->
      Choice.create (box Yolo)

    | "Quorum" ->
      Choice.create (box Quorum)

    | ugh ->
      Choice.createSnd (sprintf "%s wasn't a case in the DU named Consistency" ugh)

type ArbConfig =
  { db          : string
    batchSize   : uint16 
    endpoint    : Uri
    username    : string option
    password    : string option
    consistency : Consistency
  }

  static member empty =
    { db = ""
      batchSize = 12us
      endpoint = Uri "http://example.com"
      username = None
      password = None
      consistency = Quorum }

[<Tests>]
let uriParser =
  let subject =
    "influxdb+http://haf:w00t@host:8086/write?db=databaseName&batchSize=123"

  let expectedConfig = 
    { db          = "databaseName"
      batchSize   = 123us
      endpoint = Uri "http://host:8086/write?db=databaseName&batchSize=123"
      username    = Some "haf"
      password    = Some "w00t"
      consistency = Quorum }

  testList "uri parser" [
    testList "type coercions" [
      let data =
        [ "1", typeof<uint16>, box 1us
          "1", typeof<uint32>, box 1u
          "1", typeof<uint64>, box 1UL
          "1", typeof<int16>, box 1s
          "1", typeof<int32>, box 1
          "1", typeof<int64>, box 1L
          "s", typeof<string>, box "s"
          "http://haf.se", typeof<Uri>, box (Uri "http://haf.se")
          null, typeof<Option<string>>, box None
          "1.34", typeof<float>, box 1.34
          "1.34", typeof<Single>, box (single 1.34)
          "1.34", typeof<decimal>, box 1.34m
          "Quorum", typeof<Consistency>, box Quorum
        ]

      yield testCase "can box for fun and profit" <| fun _ ->
        for input, typ, expected in data do
          Assert.equal (Uri.convertTo typ input) expected (sprintf "should convert to %s" typ.Name)
    ]

    testList "configs" [
      testCase "parsing uri to target config" <| fun _ ->
        let actualConfig = parseConfig<ArbConfig> ArbConfig.empty subject
        Assert.equal actualConfig expectedConfig "should be correctly interpreted"

      testCase "can override with qs" <| fun _ ->
        let extraExtra = "&username=anotherUser&password=testing"
        let actualConfig = parseConfig<ArbConfig> ArbConfig.empty (subject + extraExtra)
        let expectedConfig =
          { expectedConfig with 
              endpoint = Uri ("http://host:8086/write?db=databaseName&batchSize=123" + extraExtra)
              username = Some "anotherUser"
              password = Some "testing" }
        Assert.equal actualConfig expectedConfig "qs overrides username-password next to domain"
    ]
  ]
