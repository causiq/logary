namespace Logary.Prometheus
open System
open System.Text
open System.Text.RegularExpressions
open Logary.Metric
open Suave
open Suave.Filters
open Suave.Operators
open Suave.Writers
open Hopac


type ExporterConf =
  {
    metricNameTransformer: string -> string
    metricRegistry: MetricRegistry
    webConfig: SuaveConfig
    urlPath: string
  }

module ExporterConf =
  let validNameRegex = Regex("^[a-zA-Z_][a-zA-Z0-9_]*$", RegexOptions.Compiled)
  let reservedNameRegex =  Regex("^__.*$", RegexOptions.Compiled)
  let startCharValidRegex =  Regex("^[a-zA-Z_].*$", RegexOptions.Compiled)
  let notValidAfterPartRegex =  Regex("[^a-zA-Z0-9_]", RegexOptions.Compiled)

  let defaultMetricNameTrans metricName =
    let mutable nameResult = metricName
    if String.IsNullOrEmpty nameResult then failwith "name can't be null"
    if reservedNameRegex.IsMatch nameResult then
      nameResult <- "metric_" + nameResult.Substring 2
    if not <| startCharValidRegex.IsMatch nameResult then
      nameResult <- "metric_" + nameResult
    notValidAfterPartRegex.Replace(nameResult, "_")

  let nameValidator name =
    if not <| validNameRegex.IsMatch name then
      failwithf "name (%s) should match regex %s" name (validNameRegex.ToString())
    elif reservedNameRegex.IsMatch name then
      failwithf "(%s) is invalid, names beginning with __ are reserved for internal use." name
    else name

  let create (urlPath: string) (registry: MetricRegistry) =
    {
      metricNameTransformer = defaultMetricNameTrans
      metricRegistry = registry
      webConfig = defaultConfig
      urlPath =  urlPath
    }

  let webConfig webConfig conf =
    { conf with webConfig = webConfig }


module Exporter =
  open System.Threading
  open System.Globalization
  open Logary.Internals.Chiron.Formatting
  open System.IO

  /// HELP lines may contain any sequence of UTF-8 characters (after the metric name),
  /// but the backslash and the line feed characters have to be escaped as \\ and \n, respectively
  let escapeHelp (help: string) =
    help.Replace("\\", @"\\").Replace("\n",@"\n")

  /// label_value can be any sequence of UTF-8 characters,
  /// but the backslash (\), double-quote ("), and line feed (\n) characters
  /// have to be escaped as \\, \", and \n, respectively.
  let escapeLabelValue (labelValue: string) =
    labelValue.Replace("\\", @"\\").Replace("\"",@"\""").Replace("\n",@"\n")

  let formatFloat (value: float) =
    if Double.IsPositiveInfinity value then "+Inf"
    elif Double.IsNegativeInfinity value then "-Inf"
    elif Double.IsNaN value then "Nan"
    else value.ToString(CultureInfo.InvariantCulture)

  let appendLine (sb: StringBuilder) s =
    sb.AppendLine s |> ignore

  let getTypeStr (metricInfo: MetricInfo) =
    match metricInfo with
    | Gauge _ -> "gauge"
    | Histogram _ -> "histogram"

  let getIdentifier nameTransform (name: string) (labels: Map<string,string>) =
    let labelsStr =
      if labels.Count = 0 then ""
      else
        let eachPairs =
          labels
          |> Seq.map (fun (KeyValue(name, value)) ->
            sprintf "%s=\"%s\"" (nameTransform name) (escapeLabelValue value))
        sprintf "{%s}" (String.Join(",", eachPairs))

    sprintf "%s%s" name labelsStr

  //  # HELP http_requests_total The total number of HTTP requests.
  //  # TYPE http_requests_total counter
  //  http_requests_total{method="post",code="200"} 1027 1395066363000
  //  http_requests_total{method="post",code="400"}    3 1395066363000
  //
  //  # Escaping in label values:
  //  msdos_file_access_time_seconds{path="C:\\DIR\\FILE.TXT",error="Cannot find file:\n\"FILE.TXT\""} 1.458255915e9
  //
  //  # Minimalistic line:
  //  metric_without_timestamp_and_labels 12.47
  //
  //  # A weird metric from before the epoch:
  //  something_weird{problem="division by zero"} +Inf -3982045
  //
  //  # A histogram, which has a pretty complex representation in the text format:
  //  # HELP http_request_duration_seconds A histogram of the request duration.
  //  # TYPE http_request_duration_seconds histogram
  //  http_request_duration_seconds_bucket{le="0.05"} 24054
  //  http_request_duration_seconds_bucket{le="0.1"} 33444
  //  http_request_duration_seconds_bucket{le="0.2"} 100392
  //  http_request_duration_seconds_bucket{le="0.5"} 129389
  //  http_request_duration_seconds_bucket{le="1"} 133988
  //  http_request_duration_seconds_bucket{le="+Inf"} 144320
  //  http_request_duration_seconds_sum 53423
  //  http_request_duration_seconds_count 144320
  let exportMetricInfo (sb: StringBuilder) nameTransform (metricInfo: BasicInfo * seq<MetricInfo>) =
    let basicInfo, metricInfos = metricInfo
    let metricName = nameTransform basicInfo.name
    let help = escapeLabelValue basicInfo.description
    let metricType = metricInfos |> Seq.first |> Option.map getTypeStr
    if metricType.IsSome then
      appendLine sb (sprintf "# HELP %s %s" metricName help)
      appendLine sb (sprintf "# TYPE %s %s" metricName metricType.Value)
      for metricInfo in metricInfos do
        match metricInfo with
        | Gauge gauge ->
          let identifier = getIdentifier nameTransform metricName gauge.labels
          let gaugeValueStr = gauge.gaugeValue |> formatFloat
          appendLine sb (sprintf "%s %s" identifier gaugeValueStr)
        | Histogram histogram ->
          let mutable cumulativeCount = 0.
          for (KeyValue(upperBound, count)) in histogram.bucketsInfo do
            let labelsWithLe = histogram.labels |> Map.add "le" (upperBound |> formatFloat)
            let bucketIdentifier = getIdentifier nameTransform (sprintf "%s_bucket" metricName) labelsWithLe
            cumulativeCount <- cumulativeCount + count
            appendLine sb (sprintf "%s %s" bucketIdentifier (cumulativeCount |> formatFloat))

          let sumIdentifier = getIdentifier nameTransform (sprintf "%s_sum" metricName) histogram.labels
          appendLine sb (sprintf "%s %s" sumIdentifier (histogram.sumInfo |> formatFloat))

          let countIdentifier = getIdentifier nameTransform (sprintf "%s_count" metricName) histogram.labels
          appendLine sb (sprintf "%s %s" countIdentifier (cumulativeCount |> formatFloat))


  let exportToPrometheus exportConf =
    let metricRegistry = exportConf.metricRegistry
    let metricNameTransformer = exportConf.metricNameTransformer >> ExporterConf.nameValidator
    let metricInfos = metricRegistry.getMetrictInfos ()
    let sb = new System.Text.StringBuilder ()
    metricInfos |> Seq.iter (exportMetricInfo sb metricNameTransformer)
    sb.ToString()


  let internal exportWebPart exportConf : WebPart =
    let urlPath = exportConf.urlPath
    path urlPath >=> GET  >=> setHeader "Content-Type" "text/plain; charset=utf-8" >=> request (fun r ->  Successful.OK (exportToPrometheus exportConf))


  let runAsync token conf  =
    let myApp = exportWebPart conf
    let _, srv = startWebServerAsync conf.webConfig myApp
    Async.StartAsTask(srv, cancellationToken = token)

