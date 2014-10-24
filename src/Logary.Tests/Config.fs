module Logary.Tests.Config

open System.IO
open System.Globalization

open Newtonsoft.Json

open Fuchu
open Swensen.Unquote
open TestDSL
open Fac

open NodaTime

open Logary
open Logary.Configuration
open Logary.Configuration.DTOs
open Logary.Targets
open Logary.Formatting

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

  let r1 = Rule.createForTarget "r1"
  let t1 = Noop.create Noop.empty "t1"
  let m1 = Metrics.Noop.create Metrics.Noop.empty "m1" (Duration.FromMilliseconds 500L)

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

let write (writer : TextWriter) (dto : 'a) =
  let opts = JsonFormatter.Settings ()
  let serialiser = JsonSerializer.Create opts
  serialiser.Serialize(writer, dto)

let write' (dto : 'a) =
  use sw = new StringWriter(CultureInfo.InvariantCulture)
  write sw dto
  sw.ToString()

let read (reader : TextReader) =
  let opts = JsonFormatter.Settings ()
  let serialiser = JsonSerializer.Create opts
  serialiser.Deserialize(reader, typeof<'a>)
  :?> 'a

let read' (conf : string) =
  use sr = new StringReader(conf)
  read sr

[<Tests>]
let ``loading config dtos`` =
  let json = """
{ "serviceName": "tests"
, "pollPeriod" : "00:00:00.5"
, "rules"      : [
  { "hiera"    : ".*"
  , "target"   : "console"
  , "level"    : "info" }
, { "hiera"    : "Intelliplan\\.Apps.*"
  , "target"   : "common"
  , "level"    : "verbose" }
]
, "targets"    : [
  { "name"     : "console"
  , "module"   : "Logary.Targets.Console, Intelliplan.Logary" }
]
, "metrics"    : [
  { "name"     : "common"
  , "module"   : "Logary.Metrics.WinPerfCounters.Common, Intelliplan.Logary" }
] }
"""
  testList "r/w config" [
    testCase "can write log level" <| fun _ ->
      Assert.Equal("should successfully serialise LogLevel",
                   "\"info\"",
                   write' Info)
    testCase "can read log level" <| fun _ ->
      Assert.Equal("should successfully deserialise LogLevel",
                   Info,
                   read' "\"info\"")
    testCase "can read JSON into DTOs" <| fun _ ->
      // "should successfully deserialise DTOs"
      { rules       =
          [ { hiera  = ".*"
              target = "console"
              level  = Info }
            { hiera  = "Intelliplan\\.Apps.*"
              target = "common"
              level  = Verbose }
          ]
        targets     =
          [ { name       = "console"
              ``module`` = "Logary.Targets.Console, Intelliplan.Logary" }
          ]
        metrics     =
          [ { name       = "common"
              ``module`` = "Logary.Metrics.WinPerfCounters.Common, Intelliplan.Logary" }
          ]
        serviceName ="tests"
        pollPeriod  = Duration.FromMilliseconds 500L } =? DTOs.read' json
    testCase "can read configuration from dtos - reify smoke" <| fun _ ->
      let dto = read' json
      reify dto |> ignore
    testCase "can create configuration from dtos - reify" <| fun _ ->
      let dto = read' json
      let conf = reify dto
      Assert.Equal("should have correct service name",
                   dto.serviceName, conf.metadata.serviceName)
    ]

[<Tests>]
let ``valid configs`` =
  testList "valid configs" [
    testCase "rule for metric, no noop target" <| fun _ ->
      ()
    ]