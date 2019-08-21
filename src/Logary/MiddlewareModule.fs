namespace Logary

open Logary.Internals
open System.Net
open System.Diagnostics
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
      let ambientSpanId = Span.getActiveSpanId ()
      match ambientSpanId with
      | Some ambientSpanId -> msg |> Message.setSpanId ambientSpanId |> next
      | None -> next msg

  /// Compose the list of middlewares together into a single Message->Message function.
  [<CompiledName "Compose">]
  let compose: Middleware list -> Message -> Message = function
    | [] ->
      id
    | middlewares ->
      List.foldBack (fun f composed -> f composed) middlewares id

  /// Sending logary's gauges/span or log message with metric counter to the (Prometheus) Metric registry
  let internal enableHookMetric (metricRegistry: MetricRegistry): Middleware =
    fun next message ->
      // process log message as counter
      let counterMetricConf = message |> Message.tryGetCounterMetricConf
      match counterMetricConf with
      | None -> do ()
      | Some counterMetricConf ->
        let counterMetric = GaugeConf.create counterMetricConf |> metricRegistry.registerMetric
        let counter =
          match counterMetricConf.labelNames with
          | [||] -> counterMetric.noLabels
          | labelNames ->
            // find label name's value from message's context, fields goes first
            let labelValues =
              labelNames |> Array.map (fun labelName ->
                match message |> Message.tryGetField labelName with
                | Some labelValue -> string labelValue
                | None ->
                  match message |> Message.tryGetContext labelName with
                  | Some labelValue -> string labelValue
                  | None -> sprintf "label name (%s) not found in message, but specified when metric" labelName
              )
            counterMetric.labels labelValues

        counter.inc 1.


      // process logary's gauge
      let sensorName = message.name |> PointName.format
      let gauges = message |> Message.getAllGauges
      gauges |> Seq.iter (fun (gaugeName, gauge) ->
        // TO CONSIDER: maybe make senor + gauge as one metric name, not as one metric with gauge name as label
        let gaugeMetric =
          BasicConf.create sensorName sensorName
          |> BasicConf.labelNames [| "gauge_name" |]
          |> GaugeConf.create
          |> metricRegistry.registerMetric
          |> Metric.labels [| gaugeName |]

        let gaugeValue = gauge.value.toFloat ()
        gaugeMetric.set gaugeValue
      )

      // process Logary's Span as gauge
      Message.tryGetSpanData message |> Option.iter (fun spanData ->
      let gaugeMetric =
        let metricName = message.name |> PointName.format
        GaugeConf.create(metricName, metricName)
        |> metricRegistry.registerMetric
        |> Metric.noLabels
      gaugeMetric.set spanData.elapsed.TotalSeconds)

      next message