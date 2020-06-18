module Logary.Metric.Prometheus

open System
open System.IO
open System.Text.RegularExpressions
open System.Threading.Tasks
open FSharp.Control.Tasks.Builders
open Logary.Metric
open Logary.YoLo

module internal Formatting =
  let validNameRegex = Regex("^[a-zA-Z_][a-zA-Z0-9_]*$", RegexOptions.Compiled)
  let reservedNameRegex =  Regex("^__", RegexOptions.Compiled)
  let startCharValidRegex =  Regex("^[a-zA-Z_]", RegexOptions.Compiled)
  let notValidAfterPartRegex =  Regex("[^a-zA-Z0-9_]", RegexOptions.Compiled)

  let validateName name =
    if not <| validNameRegex.IsMatch name then
      Result.Error (sprintf "name (%s) should match regex %s" name (validNameRegex.ToString()))
    elif reservedNameRegex.IsMatch name then
      Result.Error (sprintf "(%s) is invalid, names beginning with __ are reserved for internal use." name)
    else
      Result.Ok ()

  /// HELP lines may contain any sequence of UTF-8 characters (after the metric name),
  /// but the backslash and the line feed characters have to be escaped as \\ and \n, respectively
  let escapeHelp (help: string) =
    help.Replace("\\", @"\\").Replace("\n",@"\n")

  /// label_value can be any sequence of UTF-8 characters,
  /// but the backslash (\), double-quote ("), and line feed (\n) characters
  /// have to be escaped as \\, \", and \n, respectively.
  let escapeLabelValue (labelValue: string) =
    labelValue.Replace("\\", @"\\").Replace("\"",@"\""").Replace("\n",@"\n")

  let private PositiveInf = "+Inf"
  let private NegativeInf = "-Inf"
  let private NaN = "NaN"

  let private formatFloat (value: float) =
    if Double.IsPositiveInfinity value then  PositiveInf
    elif Double.IsNegativeInfinity value then NegativeInf
    elif Double.IsNaN value then NaN
    else value.ToString(Culture.invariant)

  /// Does not write newlines
  let private writeFloat (writer: TextWriter) (value: float): Task =
    writer.WriteAsync(formatFloat value)

  let private getIdentifier nameMetric metricName (labels: Map<string, string>): string =
    if labels.Count = 0 then metricName else

    labels
      |> Seq.map (fun (KeyValue (name, value)) -> nameMetric name, escapeLabelValue value)
      |> Seq.filter (fun (name, _) -> validateName name |> function Ok _ -> true | _ -> false)
      |> Seq.map (fun (name, value) -> sprintf "%s=\"%s\"" name value)
      |> String.concat ","
      |> sprintf "%s{%s}" metricName

  // Does not write newlines
  let formatGuage (writer: TextWriter) nameMetric metricName (gauge: GaugeInfo): Task =
    task {
      match validateName metricName with
      | Ok _ ->
        do! writer.WriteAsync (getIdentifier nameMetric metricName gauge.labels)
        do! writer.WriteAsync " "
        do! writeFloat writer gauge.value
      | Error message ->
        eprintfn "# Error formatting gauge: '%s'" message
    } :> _

  // Writes newlines inside only
  let formatHistogram (writer: TextWriter) nameMetric metricName (histogram: HistogramInfo): Task =
    task {
      match validateName metricName with
      | Ok _ ->
        let mutable cumulativeCount = 0.

        // write buckets:
        for KeyValue(upperBound, count) in histogram.buckets do
          let labelsWithLe = histogram.labels |> Map.add "le" (formatFloat upperBound)
          let bucketIdentifier = getIdentifier nameMetric (sprintf "%s_bucket" metricName) labelsWithLe
          cumulativeCount <- cumulativeCount + count
          do! writer.WriteAsync bucketIdentifier
          do! writer.WriteAsync ' '
          do! writer.WriteAsync(formatFloat cumulativeCount)
          do! writer.WriteLineAsync()

        // write sum:
        let sumIdentifier = getIdentifier nameMetric (sprintf "%s_sum" metricName) histogram.labels
        do! writer.WriteAsync sumIdentifier
        do! writer.WriteAsync ' '
        do! writer.WriteAsync(formatFloat histogram.sum)
        do! writer.WriteLineAsync()

        // write count:
        let countIdentifier = getIdentifier nameMetric (sprintf "%s_count" metricName) histogram.labels
        do! writer.WriteAsync countIdentifier
        do! writer.WriteAsync ' '
        do! writer.WriteAsync(cumulativeCount |> formatFloat)

      | Error message ->
        eprintfn "# Error formatting histogram: '%s'" message
    } :> _

open Formatting


/// Default implementation of the metric naming.
let defaultMetricNamer (metricName: string) =
  if String.IsNullOrEmpty metricName then nullArg "metricName"

  let mutable nameResult = metricName
  if reservedNameRegex.IsMatch nameResult then
    nameResult <- "metric_" + nameResult.Substring 2

  if not <| startCharValidRegex.IsMatch nameResult then
    nameResult <- "metric_" + nameResult

  notValidAfterPartRegex.Replace(nameResult, "_")

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
let writeMetric (writer: TextWriter, nameMetric: string -> string) ((basicInfo, metrics): BasicInfo * seq<MetricInfo>): Task =
  let name = nameMetric basicInfo.name
  let help = escapeLabelValue basicInfo.description
  let metricType = Seq.tryHead metrics |> Option.map (fun m -> m.kind)

  let writeHelp = lazy (task {
    do! writer.WriteLineAsync(sprintf "# HELP %s %s" name help)
    do! writer.WriteLineAsync(sprintf "# TYPE %s %s" name metricType.Value)
  })

  task {
    for metric in metrics do
      if not (writeHelp.IsValueCreated) then do! writeHelp.Value
      if metric.unit.name.IsSome then do! writer.WriteLineAsync(sprintf "# Unit: %s" metric.unit.name.Value)

      match metric with
      | Gauge gauge ->
        do! formatGuage writer nameMetric name gauge
      | Histogram histogram ->
        do! formatHistogram writer nameMetric name histogram

      do! writer.WriteLineAsync()
  } :> _
