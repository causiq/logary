module Logary.Metrics.WinPerfCounters

open System.Diagnostics
open Hopac
open Logary
open Logary.Metric
open Logary.Internals
open Logary.Metrics.WinPerfCounter
open Logary.Metrics.AllWinPerfCounters

module Common =

  let ofPerfCounters counters =
    counters
    |> Array.map (fun counter -> counter, toWindowsCounter counter)
    |> Array.filter (snd >> Option.isSome)
    |> Array.map (fun (counter, pc) -> counter, Option.get pc)

  let system =
    [| System.``Context Switches/sec``
       System.``Processor Queue Length`` |]
    |> ofPerfCounters

  let cpuTime =
    [| Processor.``% Processor Time``
       Processor.``% User Time``
       Processor.``% Interrupt Time``
       Processor.``% Privileged Time``
       Processor.``Interrupts/sec``
       Processor.``% Idle Time`` 
    |]
    |> Array.map (fun f -> f (Some KnownInstances._Total))
    |> ofPerfCounters

  let proc instance =
    [| Process.``% Processor Time`` instance
       Process.``% User Time`` instance
       Process.``% Privileged Time`` instance
       Process.``Virtual Bytes`` instance
    |]
    |> ofPerfCounters

  /// Useful ASP.Net counters, from
  /// http://technet.microsoft.com/en-us/library/cc778343%28v=ws.10%29.aspx
  ///
  /// You can either pass the app domain instance process name, or you could use
  /// WinPerfCounter.pidInstance () to get your own instance name. If you have
  /// multiple processes with the same name executing, you will need to
  /// discriminate by name.
  ///
  /// Use WinPerfCounter.pidInstance () to get the current process' instance.
  let recommended instance =
    Array.concat [
      cpuTime
      proc instance
    ]

  let recommendedProc () =
    let procInstance = Helpers.pidInstance ()
    Array.concat [
      cpuTime
      proc procInstance
    ]

  let byCategory pcc =
    Category.create pcc
    // gets all the instances for this performance counter category
    |> Option.map (fun pcc -> pcc, Category.instances pcc)
    // transform the list of instances into a list
    |> Option.map (fun (pcc, instances) ->
      instances
      // find all counters for these instances (instance = actual GPU instance)
      |> Array.collect (WinPerfCounter.list pcc)
      // get the Diagnostics perf counters for them
      |> ofPerfCounters)
    
/// A list of "common" performance metrics.
open WinPerfCounter

let private ofCounters (counters : (WinPerfCounter * PerformanceCounter) []) =
  let reducer state msg = state
  let ticker (state : (WinPerfCounter * PerformanceCounter) []) =
    state, state |> Seq.map Helpers.toValue |> List.ofSeq
  Metric.create reducer counters ticker

/// The "GPU" category is installed by the nVIDIA drivers
let tryGPUMetric _ : Job<Metric> option =
  Common.byCategory "GPU" |> Option.map ofCounters
  
let gpuMetrics _ : Job<Metric> =
  match tryGPUMetric () with
  | None ->
    failwith "No GPUs configured (or drivers thereof) on this machine"
  | Some metric ->
    metric

let tryNVidiaGPUMetric _ : Job<Metric> option =
  Common.byCategory "NVIDIA GPU" |> Option.map ofCounters
  
let nvidiaMetrics pn : Job<Metric> =
  match tryNVidiaGPUMetric () with
  | None ->
    failwith "No nVIDIA GPUs configured (or drivers thereof) on this machine"

  | Some metric ->
    metric

let cpuTime _ : Job<Metric> =
  ofCounters Common.cpuTime

let tryCpuInformation _ : Job<Metric> option =
  Common.byCategory "Processor Information" |> Option.map ofCounters

let cpuInformation _ : Job<Metric> =
  match tryCpuInformation () with
  | None ->
    failwith "The performance counter category 'Processor Information' wasn't available on this machine"

  | Some metric ->
    metric

let tryNetworkInterface _ : Job<Metric> option =
  Common.byCategory "Network Interface" |> Option.map ofCounters

let networkInterface pn =
  match tryNetworkInterface pn with
  | None ->
    failwith "The performance counter category 'Network Interface' wasn't available on this machine"

  | Some metric ->
    metric