module Logary.TOML.ParseMetric

let sampleConfig = """
[logary]
service = "Logary.TOML.Tests"
poll    = 500

[targets]

  [targets.console1]

  [targets.riemann]
  endpoint = "10.4.5.2:1935"
  tags     = [ "sample", "tests" ]
  hostname = "node12-cluster2"

[rules]

  [rules.console]

  [rules.riemann]
  hiera = ".*"
  level = "Info"
"""

open Fuchu

open Logary
open Logary.Configuration
open Logary.Targets

[<Tests>]
let correctParse =
  testList "can parse the TOML string correctly" [
    testCase "throws on missing service" <| fun _ ->
      Assert.Raise("should not be missing service key", typeof<MissingService>,
                   fun () -> confLogaryString "" |> ignore)
    testCase "can parse valid" <| fun _ ->
      let parsed = confLogaryString sampleConfig
      parsed |> ignore
    testCase "parses targets, rules" <| fun _ ->
      let parsed = confLogaryString sampleConfig
      Assert.Equal("should have correct targets",
                   [ "console1", (Console.create Console.empty "console1", None)
                     "riemann", (Riemann.create Riemann.empty "riemann", None) ] |> Map.ofList,
                   parsed.targets)
      Assert.Equal("should have correct rules",
                   [ Rule.createForTarget "console1"; { Rule.createForTarget "riemann" with level = Info } ],
                   parsed.rules)
    ]