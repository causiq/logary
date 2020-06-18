/// Conventional metrics.
///
/// https://prometheus.io/docs/practices/naming/
module Logary.Metric.Conventions

open Logary

let AspNetCoreLabels = [
  "code"
  "method"
  "controller"
  "action"
]

let http_server_request_count =
  GaugeConf.create("http_client_request_count",
                   "A counter of the ASP.Net Core server requests",
                   U.Seconds,
                   AspNetCoreLabels)

let http_server_request_duration_seconds =
  HistogramConf.create("http_server_request_duration_seconds",
                       "The histogram of the duration of all received HTTP requests. Specified in seconds.",
                       U.Seconds,
                       HistogramConf.defaultBuckets,
                       AspNetCoreLabels)

let http_client_request_count =
  GaugeConf.create("http_client_request_count",
                   "A counter of the client requests",
                   U.Seconds)

let http_client_request_duration_seconds =
  HistogramConf.create("http_client_request_duration_seconds",
                       "The duration of sent HTTP requests in seconds",
                       U.Seconds,
                       HistogramConf.defaultBuckets)