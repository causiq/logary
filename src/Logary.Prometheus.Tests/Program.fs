// Learn more about F# at http://fsharp.org

open System
open System.Net
open System.Threading
open Expecto
open Logary
open Logary.Prometheus
open Logary.Metric
open Expecto.Flip
open System.Net.Http
open Hopac

module Exporter =

  open Suave


  let checkTextProtocol expected actual =
    expected |> String.split '\n' |> Expect.containsAll "should exported to prometheus text protocol" (actual |> String.trimStart |> String.split '\n')


  [<Tests>]
  let tests = testList "Logary.Prometheus" [


    testCaseJob "export to prometheus text protocol" <| job {
      let httpClient = new HttpClient()
      httpClient.BaseAddress <- Uri("http://127.0.0.1:8080/")
      let exportedTextJob = Job.liftTask (fun (endpoint:string) -> httpClient.GetStringAsync(endpoint)) "metrics"

      let webConfig = defaultConfig.withBindings [HttpBinding.create HTTP IPAddress.Loopback 8081us]

      let cts = new CancellationTokenSource()
      use cancelWebServer =  { new IDisposable with member x.Dispose() = cts.Cancel () }

      ExporterConf.create "/metrics" LogManager.DefaultMetricRegistry
      |> Exporter.runAsync cts.Token
      |> ignore

      let gauge =
        GaugeConf.create {name = "time_latancy"; description = "test time latancy"; labelNames = [| "endpoint" |]}
        |> GaugeConf.withHistogram [|50.; 100.; 500.;|]
        |> LogManager.DefaultMetricRegistry.registerMetric

      (gauge.labels [| "/users" |]).inc 100.
      (gauge.labels [| "/users" |]).inc 200.
      (gauge.labels [| "/books" |]).inc 50.
      (gauge.labels [| "/books" |]).dec 20.

      let! exportedText = exportedTextJob


      let expected = """
# HELP time_latancy test time latancy
# TYPE time_latancy gauge
time_latancy{endpoint="/books"} 30
time_latancy{endpoint="/users"} 300
# HELP time_latancy_histogram histogram of gauge:`test time latancy`
# TYPE time_latancy_histogram histogram
time_latancy_histogram_bucket{endpoint="/books",le="50"} 2
time_latancy_histogram_bucket{endpoint="/books",le="100"} 2
time_latancy_histogram_bucket{endpoint="/books",le="500"} 2
time_latancy_histogram_bucket{endpoint="/books",le="+Inf"} 2
time_latancy_histogram_sum{endpoint="/books"} 80
time_latancy_histogram_count{endpoint="/books"} 2
time_latancy_histogram_bucket{endpoint="/users",le="50"} 0
time_latancy_histogram_bucket{endpoint="/users",le="100"} 1
time_latancy_histogram_bucket{endpoint="/users",le="500"} 2
time_latancy_histogram_bucket{endpoint="/users",le="+Inf"} 2
time_latancy_histogram_sum{endpoint="/users"} 400
time_latancy_histogram_count{endpoint="/users"} 2
"""

      checkTextProtocol expected exportedText

      (gauge.labels [| "/users" |]).dec 50.
      (gauge.labels [| "/books" |]).set 250.

      let expected = """
# HELP time_latancy test time latancy
# TYPE time_latancy gauge
time_latancy{endpoint="/books"} 250
time_latancy{endpoint="/users"} 250
# HELP time_latancy_histogram histogram of gauge:`test time latancy`
# TYPE time_latancy_histogram histogram
time_latancy_histogram_bucket{endpoint="/books",le="50"} 2
time_latancy_histogram_bucket{endpoint="/books",le="100"} 2
time_latancy_histogram_bucket{endpoint="/books",le="500"} 3
time_latancy_histogram_bucket{endpoint="/books",le="+Inf"} 3
time_latancy_histogram_sum{endpoint="/books"} 330
time_latancy_histogram_count{endpoint="/books"} 3
time_latancy_histogram_bucket{endpoint="/users",le="50"} 0
time_latancy_histogram_bucket{endpoint="/users",le="100"} 1
time_latancy_histogram_bucket{endpoint="/users",le="500"} 3
time_latancy_histogram_bucket{endpoint="/users",le="+Inf"} 3
time_latancy_histogram_sum{endpoint="/users"} 650
time_latancy_histogram_count{endpoint="/users"} 3
"""
      let! exportedText = exportedTextJob

      checkTextProtocol expected exportedText

    }




    testCase "dfault metric name transformer" <| fun () ->
      ExporterConf.defaultMetricNameTrans "items in queue" |> Expect.equal "should transform" "items_in_queue"
      ExporterConf.defaultMetricNameTrans "logary.some.module.function.duration" |> Expect.equal "should transform" "logary_some_module_function_duration"
      ExporterConf.defaultMetricNameTrans "4just some@test" |> Expect.equal "should transform" "metric_4just_some_test"
      ExporterConf.defaultMetricNameTrans "__just some test" |> Expect.equal "should transform" "metric_just_some_test"

  ]




[<EntryPoint>]
let main args =
  Tests.runTestsInAssembly defaultConfig args
