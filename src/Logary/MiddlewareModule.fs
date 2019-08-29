namespace Logary

open System.Net
open System.Diagnostics
open Logary.Trace
open Logary.Internals
open Logary.Metric

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Middleware =

  /// This is the identity middleware.
  [<CompiledName "Identity">]
  let identity: Middleware =
    fun next msg -> next msg

  /// Sets the host name as a context value
  [<CompiledName "Host">]
  let host host: Middleware =
    fun next msg ->
      Message.setContext KnownLiterals.HostContextName host msg
      |> next

  /// Sets the host name as a context value
  [<CompiledName "DnsHost">]
  let dnsHost: Middleware =
    fun next msg ->
      Message.setContext KnownLiterals.HostContextName (Dns.GetHostName()) msg
      |> next

  /// Sets the service name as a context value
  [<CompiledName "Service">]
  let service (name: string): Middleware =
    fun next msg ->
      Message.setContext KnownLiterals.ServiceContextName name msg
      |> next

  let private pn = Process.GetCurrentProcess()

  /// Sets the current process' name as a context value
  [<CompiledName "ProcessName">]
  let processName: Middleware =
    fun next msg ->
      Message.setContext "processName" pn.ProcessName msg
      |> next

  /// Sets the current process' id as a context value
  [<CompiledName "ProcessId">]
  let processId: Middleware =
    fun next msg ->
      Message.setContext "processId" pn.Id msg
      |> next

  /// Always sets this context value.
  [<CompiledName "Context">]
  let context name value =
    fun next msg ->
      Message.setContext name value msg
      |> next

  [<CompiledName "AmbientSpanId">]
  let ambientSpanId: Middleware =
    fun next msg ->
      match ActiveSpan.getSpan () with
      | Some ambientSpanId ->
        msg |> Message.setSpanId ambientSpanId |> next
      | None ->
        next msg

  /// Compose the list of middlewares together into a single Message->Message function.
  [<CompiledName "Compose">]
  let compose: Middleware list -> Message -> Message = function
    | [] ->
      id
    | middlewares ->
      List.foldBack (fun f composed -> f composed) middlewares id

  /// You can enable this middleware if you want Gauges and other Metrics coming as messages in Logary (e.g. via
  /// the Trace functionality), to be emitted into the `MetricRegistry`.
  ///
  /// Will send Logary's Gauges/SpanData or a plain log message with a metric counter to the (Prometheus) Metric
  /// registry.
  ///
  /// Will try to match Spans with conventions and map those to the Prometheus conventions:
  /// https://github.com/open-telemetry/opentelemetry-specification/blob/master/specification/data-semantic-conventions.md
  /// to:
  /// https://github.com/prometheus/client_ruby#histogram
  let internal metricsToRegistry (registry: MetricRegistry): Middleware =
    /// Find label name's value from Message's context, fields goes first
    let getLabelValues message labelName =
      message
        |> Message.tryGetField labelName
        |> Option.orElseWith (fun () -> message |> Message.tryGetContext labelName)
        |> Option.map string
        |> Option.defaultWith (fun () -> sprintf "The label \"%s\" was not found in the Message, but was specified in the BasicConf given to Message.enableCounterMetric." labelName)

    let processMessageAsCounter message conf =
      let counterMetric = GaugeConf.create conf |> registry.getOrCreate
      let counter =
        match conf.labelNames with
        | [||] ->
          counterMetric.noLabels
        | labelNames ->
          labelNames
            |> Array.map (getLabelValues message)
            |> counterMetric.labels
      counter.inc 1.

    let processLogaryGauge sensorName (gaugeName, gauge: Logary.Gauge) =
      // TO CONSIDER: maybe make sensor + gauge as one metric name, not as one metric with gauge name as label
      let gaugeMetric =
        GaugeConf.create(sensorName, "Gauge via " + sensorName, [| "gauge_name" |])
        |> registry.getOrCreate
        |> Metric.labels [| gaugeName |]

      let gaugeValue = gauge.value.toFloat ()
      gaugeMetric.set gaugeValue

    let processSpanAsGauge (span: SpanData) =
      let via = span.resource |> Option.map (fun name -> " via " + name.ToString()) |> Option.defaultValue ""
      let gauge = GaugeConf.create(span.label, "Span" + via) |> registry.getOrCreate
      gauge.noLabels.set span.elapsed.TotalSeconds

    /// https://github.com/open-telemetry/opentelemetry-specification/blob/master/specification/data-semantic-conventions.md
    let processSpanAsConvention (span: SpanData) =
      let httpLabels () =
         List.concat [
           span.tryGetInt "http.status_code" |> Option.map (fun code -> ("code", string code) :: []) |> Option.defaultValue []
           span.tryGetString "http.method" |> Option.map (fun m -> ("method", m) :: []) |> Option.defaultValue []
           span.tryGetString "http.route" |> Option.map (fun r -> ("route", r) :: []) |> Option.defaultValue []
           span.tryGetBool "error" |> Option.map (fun error -> ("error", string error) :: []) |> Option.defaultValue []
         ]
         |> Map

      let histO, labels =
        match span.kind, span.``component`` with
        | SpanKind.Server, Some "http" ->
          registry.getOrCreate Conventions.http_server_request_duration_seconds |> Some,
          httpLabels ()

        | SpanKind.Client, Some "http" ->
          registry.getOrCreate Conventions.http_client_request_duration_seconds |> Some,
          httpLabels ()

        | _ ->
          None,
          Map.empty

      histO |> Option.iter (fun histogram ->
      histogram.labels(labels).observe span.elapsed.TotalSeconds)

    /// https://prometheus.io/docs/practices/naming/
    /// ...should have a (single-word) application prefix relevant to the domain the metric belongs to. The prefix is
    /// sometimes referred to as namespace by client libraries. For metrics specific to an application, the prefix is
    /// usually the application name itself. Sometimes, however, metrics are more generic, like standardized metrics
    /// exported by client libraries.
    ///
    /// Examples:
    /// - `prometheus_notifications_total` (specific to the Prometheus server)
    /// - `process_cpu_seconds_total` (exported by many client libraries)
    /// - `http_server_request_duration_seconds` (for all HTTP requests)
    ///
    /// Also tries to extract convention-based global gauges, see
    /// https://github.com/open-telemetry/opentelemetry-specification/blob/master/specification/data-semantic-conventions.md
    let processLogarySpan (span: SpanData) =
      processSpanAsGauge span
      processSpanAsConvention span

    fun next message ->
      Message.tryGetCounterMetricConf message |> Option.iter (processMessageAsCounter message)

      let sensorName = message.name.ToString()
      Message.getAllGauges message |> Seq.iter (processLogaryGauge sensorName)
      Message.tryGetSpanData message |> Option.iter processLogarySpan

      next message