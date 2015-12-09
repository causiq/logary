module Logary.Tests.WinPerfCountersMetrics

open Fuchu
open Swensen.Unquote

open TestDSL

open NodaTime

open Logary
open Logary.Internals
open Logary.Metrics.WinPerfCounters

let run = Async.RunSynchronously
let nullInfo = { serviceName = "tests"; logger = NullLogger() }

let withMetric label f =
  let mc = create Common.cpuTimeConf "cpuTime" (Duration.FromMilliseconds 200L)
  let inst = mc.initer nullInfo
  testCase label <| fun _ ->
    try
      f mc inst
    finally
      inst |> Metric.shutdown |> run |> ignore

[<Tests>]
let tests =
  testList "WinPertCounters metrics" [
    withMetric "initialising" <| fun mc inst ->
      Assert.Equal("eq cpuTime", mc.name, "cpuTime")

    withMetric "get empty list of values"  <| fun mc inst ->
      (because "getting empty list of values" <| fun _ ->
        match inst |> Metric.getValue [] |> run with
        | [] -> ()
        | xs -> Tests.failtestf "got %A but expected []" xs)
      |> thatsIt

    withMetric "get CPU values" <| fun mc inst ->
      (because "has data points" <| fun _ ->
        match inst |> Metric.getDataPoints |> run with
        | [] -> Tests.failtest "no dps"
        | dps -> inst |> Metric.getValue dps |> run, dps.Length)
      |> should' (fulfil (fun (values, expectedLen) ->
                            "should have value for each", values.Length = expectedLen))
      |> thatsIt
    ]