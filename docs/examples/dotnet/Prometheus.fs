module A =
  open Logary.Metric

  let logger = Log.create "xxx.yyy"

  let parseConfigFailedCounter =
    GaugeConf.create {name="xxx_yyy_parse_config_faild_total"; description= "the total number for xxx.yyy.parseConfig faild"; labelNames = [| "path" |]}
    |> LogManager.DefaultMetricRegistry.registerMetric

  // add gauge metric and histogram with buckets [| 0.001; 0.005; 0.01; 0.025; 0.05; 0.1; 1.0|] in the same time
  let parseConfigGaugeWithHistogram =
    GaugeConf.create {name="xxx_yyy_parse_config_latency_seconds"; description= "xxx.yyy.parseConfig latency in seconds"; labelNames = [||]}
    |> GaugeConf.withHistogram [| 0.001; 0.005; 0.01; 0.025; 0.05; 0.1; 1.0|]
    |> LogManager.DefaultMetricRegistry.registerMetric
    |> Metric.noLabels


  // using default buckets:  [| 0.005; 0.01; 0.025; 0.05; 0.075; 0.1; 0.25; 0.5; 0.75; 1.; 2.5; 5.0; 7.5; 10. |]
  let parseConfigLatencyHistogram =
    HistogramConf.create("xxx_yyy_parse_config_latency_seconds", "xxx.yyy.parseConfig latency in seconds")
    |> LogManager.DefaultMetricRegistry.registerMetric
    |> Metric.noLabels

  // using hookup and shortcut style
  let parseConfigWithShortcut path =
    try
      // if we hook up metric at logary's conf, then there is no need to do other things here.
      // span will be treated as gauges when span has been finished.
      use traceSpan =
        logger.buildSpan()
        |> Span.setMessage (eventX "time usage about parse config with {path}" >> setField "path" path)
        |> Span.start

      // do some work here
      failwith "not implement"
      
    with ex ->
      eventX "parse config from {path} failed"
      >> setField "path" path
      >> enableCounterMetric {name="xxx_yyy_parse_config_faild_total"; description= "the total number for xxx.yyy.parseConfig faild"; labelNames = [| "path" |]}
      |> logger.error

  // using metric as a separate instance style
  let parseConfig path =
    try
      let traceSpan =
        logger.buildSpan()
        |> Span.setMessage (eventX "time usage about parse config with {path}" >> setField "path" path)
        |> Span.start

      // do some work here
      failwith "not implement"


      let spanLog = traceSpan.finish id
      // gauge with histogram like below
      parseConfigGaugeWithHistogram.set (Duration.FromTicks(spanLog.duration).TotalSeconds)
      // or metric only histogram like below
      parseConfigLatencyHistogram.observe (Duration.FromTicks(spanLog.duration).TotalSeconds)

    with ex ->
      eventX "parse config from {path} failed"
      >> setField "path" path
      |> logger.error

      (parseConfigFailedCounter.labels [| path |]).inc 1.