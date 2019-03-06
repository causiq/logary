// Learn more about F# at http://fsharp.org

open System
open System.Net
open System.Threading
open Expecto
open Logary
open Logary.Prometheus
open Logary.Metric


module Exporter =

  open Suave

  [<Tests>]
  let tests = testList "Logary.Prometheus" [


    testCase "" <| fun _ ->
      let webConfig = defaultConfig.withBindings [HttpBinding.create HTTP IPAddress.Loopback 8081us]

//      ExporterConf.create "/metrics" LogManager.DefaultMetricRegistry
//      |> ExporterConf.webConfig webConfig
//      |> Exporter.run





      ()
  ]









[<EntryPoint>]
let main args =
//  Tests.runTestsInAssembly defaultConfig args
  let cts = new CancellationTokenSource()
  ExporterConf.create "/metrics" LogManager.DefaultMetricRegistry
  |> Exporter.runAsync cts.Token
  |> ignore

  let gauge =
    GaugeConf.create {name = "time_latancy"; description = "test time latancy"; labelNames = [| "endpoint" |]}
    |> GaugeConf.withHistogram [|10.; 20.; 50.; 100.; 200.; 500.;|]
    |> LogManager.DefaultMetricRegistry.registerMetric

  (gauge.labels [| "/users" |]).inc 100.
  (gauge.labels [| "/users" |]).inc 200.
  (gauge.labels [| "/users" |]).inc 50.
  (gauge.labels [| "/books" |]).inc 50.
  (gauge.labels [| "/books" |]).dec 20.
  (gauge.labels [| "/books" |]).set 250.

  printfn "done"
  Console.ReadKey ()
  1