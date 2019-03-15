namespace Logary.Prometheus.Exporter

open System
open Suave
open Suave.Filters
open Suave.Operators
open Suave.Writers
open Logary.Metric
open Logary.Prometheus

type ExporterConf =
  {
    metricNameTransformer: string -> string
    metricRegistry: MetricRegistry
    webConfig: SuaveConfig
    urlPath: string
  }

module ExporterConf =

  let create (urlPath: string) (registry: MetricRegistry) =
    {
      metricNameTransformer = Formatting.defaultMetricNameTrans
      metricRegistry = registry
      webConfig = defaultConfig
      urlPath =  urlPath
    }

  let webConfig webConfig conf =
    { conf with webConfig = webConfig }


module Exporter =
  open System.Threading

  let exportToPrometheus exportConf =
    let metricRegistry = exportConf.metricRegistry
    let metricNameTransformer = exportConf.metricNameTransformer >> Formatting.validateName
    let metricInfos = metricRegistry.getMetricInfos ()
    let sb = new System.Text.StringBuilder ()
    metricInfos |> Seq.iter (Formatting.formatMetricInfo sb metricNameTransformer)
    sb.ToString()

  let internal exportWebPart exportConf : WebPart =
    let urlPath = exportConf.urlPath
    path urlPath >=> GET
      >=> setHeader "Content-Type" "text/plain; charset=utf-8"
      >=> request (fun r ->  Successful.OK (exportToPrometheus exportConf))


  let run conf  =
    let cts = new CancellationTokenSource()
    let myApp = exportWebPart conf
    let suaveConfig = { conf.webConfig with cancellationToken = cts.Token }
    let _, srv = startWebServerAsync suaveConfig myApp
    Async.Start (srv, cancellationToken = cts.Token)
    { new IDisposable with member x.Dispose() = cts.Cancel () }

