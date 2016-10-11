module Logary.Metrics.WinPerfCounters

open System.Diagnostics
open Hopac
open Logary
open Logary.Metric
open Logary.Internals
open Logary.Metrics.WinPerfCounter
open Logary.Metrics.AllWinPerfCounters

let ofPerfCounters counters =
  counters
  |> Array.map (fun counter -> counter, toWindowsCounter counter)
  |> Array.filter (snd >> Option.isSome)
  |> Array.map (fun (counter, pc) -> counter, Option.get pc)

let private forInstances instancesFactory unitsAndCounters =
  let instances = instancesFactory ()
  unitsAndCounters |> Array.collect (fun (units, create) ->
    instances |> Array.choose (function
      | None ->
        None
      | instance ->
        Some (units, create instance)))

let private forInstance instanceFactory unitsAndCounters =
  forInstances (fun () -> [| instanceFactory () |]) unitsAndCounters

let systemCounters () =
  Array.concat [
    [| Div (Scalar, Seconds), System.``Context Switches/sec``
       Scalar,                System.``Processor Queue Length`` |]

    [| Percent,               Processor.``% Processor Time``
       Percent,               Processor.``% User Time``
       Percent,               Processor.``% Interrupt Time``
       Percent,               Processor.``% Privileged Time``
       Div (Scalar, Seconds), Processor.``Interrupts/sec``
       Percent,               Processor.``% Idle Time`` 
    |] |> Array.map (fun (units, f) -> units, f (Some KnownInstances._Total))

    [| Bytes,                 Memory.``Available Bytes``
       Div (Scalar, Seconds), Memory.``Page Faults/sec``
       Scalar,                Memory.``Free System Page Table Entries``
       Bytes,                 Memory.``Pool Paged Bytes``
       Bytes,                 Memory.``Pool Nonpaged Bytes``
       Bytes,                 Memory.``Pool Paged Resident Bytes``
       Div (Scalar, Seconds), Memory.``Pages Input/sec``
       Div (Scalar, Seconds), Memory.``Pages Output/sec``
       Div (Scalar, Seconds), Memory.``Pages/sec``
    |]

    // https://blogs.technet.microsoft.com/askcore/2012/03/16/windows-performance-monitor-disk-counters-explained/
    [| Div (Seconds, Scalar), LogicalDisk.``Avg. Disk sec/Read``
       Div (Seconds, Scalar), LogicalDisk.``Avg. Disk sec/Write``
       Scalar,                LogicalDisk.``Avg. Disk Read Queue Length``
       Scalar,                LogicalDisk.``Avg. Disk Write Queue Length``
       Div (Scalar, Seconds), LogicalDisk.``Disk Reads/sec``
       Div (Scalar, Seconds), LogicalDisk.``Disk Writes/sec``
       Div (Bytes, Seconds),  LogicalDisk.``Disk Read Bytes/sec``
       Div (Bytes, Seconds),  LogicalDisk.``Disk Write Bytes/sec``
       Bytes,                 LogicalDisk.``Free Megabytes``
       Percent,               LogicalDisk.``% Idle Time``
       Percent,               LogicalDisk.``% Free Space``
    |] |> forInstances LogicalDisk.instances

    [| Div (Seconds, Scalar), PhysicalDisk.``Avg. Disk sec/Read``
       Div (Seconds, Scalar), PhysicalDisk.``Avg. Disk sec/Write``
       Scalar,                PhysicalDisk.``Avg. Disk Read Queue Length``
       Scalar,                PhysicalDisk.``Avg. Disk Write Queue Length``
       Div (Scalar, Seconds), PhysicalDisk.``Disk Reads/sec``
       Div (Scalar, Seconds), PhysicalDisk.``Disk Writes/sec``
       Div (Bytes, Seconds),  PhysicalDisk.``Disk Read Bytes/sec``
       Div (Bytes, Seconds),  PhysicalDisk.``Disk Write Bytes/sec``
       Percent,               PhysicalDisk.``% Idle Time``
    |] |> forInstances PhysicalDisk.instances

    [| Scalar,                Objects.Processes
       Scalar,                Objects.Mutexes
       Scalar,                Objects.Threads
       Scalar,                Objects.Semaphores
    |]
  ]
  |> Array.map (fun (units, wpc) -> wpc |> WinPerfCounter.setUnit units)
  |> ofPerfCounters

let appCounters () =
  let instance = Helpers.pidInstance ()
  let clrMem counter inst = WinPerfCounter.create(".NET CLR Memory", counter, inst)
  let clrExn counter inst = WinPerfCounter.create(".NET CLR Exceptions", counter, inst)
  let clrLocks counter inst = WinPerfCounter.create(".NET CLR LocksAndThreads", counter, inst)
  Array.concat [
    [| Percent, Process.``% Processor Time``
       Percent, Process.``% User Time``
       Percent, Process.``% Privileged Time``
       Bytes,   Process.``Virtual Bytes``
       Bytes,   Process.``Private Bytes``
       Bytes,   Process.``Working Set``
       Scalar,  Process.``IO Data Operations/sec``
       Scalar,  Process.``IO Other Operations/sec``
    |]

    [| Bytes,                 "# Bytes in all Heaps"
       Bytes,                 "Gen 0 heap size"
       Bytes,                 "Gen 1 heap size"
       Bytes,                 "Gen 2 heap size"
       Bytes,                 "Large Object Heap size"
       Div (Bytes, Seconds),  "Allocated Bytes/sec"
       Percent,               "% Time in GC"
       Scalar,                "# of Pinned Objects"
    |] |> Array.map (fun (units, counter) -> units, clrMem counter)

    [| Div (Scalar, Seconds), "# of Exceps Thrown / Sec"
       Div (Scalar, Seconds), "# of Filters / Sec"
       Div (Scalar, Seconds), "# of Finallys / Sec"
       Div (Scalar, Seconds), "Throw to Catch Depth / Sec"
    |] |> Array.map (fun (units, counter) -> units, clrExn counter)

    [| Scalar,                "# of current logical Threads"
       Scalar,                "# of current physical Threads"
       Div (Scalar, Seconds), "Contention Rate / Sec"
       Div (Scalar, Seconds), "Queue Length / sec"
    |] |> Array.map (fun (units, counter) -> units, clrLocks counter)
  ]
  |> forInstance Helpers.pidInstance
  |> Array.map (fun (units, wpc) -> wpc |> WinPerfCounter.setUnit units)
  |> ofPerfCounters

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
    
let private ofCounters (counters : (WinPerfCounter * PerformanceCounter) []) =
  let reducer state msg = state
  let ticker (state : (WinPerfCounter * PerformanceCounter) []) =
    state, state |> Seq.map Helpers.toValue |> List.ofSeq
  Metric.create reducer counters ticker

/// The "GPU" category is installed by the nVIDIA drivers
let tryGPUMetric _ : Job<Metric> option =
  byCategory "GPU" |> Option.map ofCounters
  
let gpuMetrics _ : Job<Metric> =
  match tryGPUMetric () with
  | None ->
    failwith "No GPUs configured (or drivers thereof) on this machine"
  | Some metric ->
    metric

let tryNVidiaGPUMetric _ : Job<Metric> option =
  byCategory "NVIDIA GPU" |> Option.map ofCounters
  
let nvidiaMetrics pn : Job<Metric> =
  match tryNVidiaGPUMetric () with
  | None ->
    failwith "No nVIDIA GPUs configured (or drivers thereof) on this machine"

  | Some metric ->
    metric

let tryCpuInformation _ : Job<Metric> option =
  byCategory "Processor Information" |> Option.map ofCounters

let cpuInformation _ : Job<Metric> =
  match tryCpuInformation () with
  | None ->
    failwith "The performance counter category 'Processor Information' wasn't available on this machine"

  | Some metric ->
    metric

let tryNetworkInterface _ : Job<Metric> option =
  byCategory "Network Interface" |> Option.map ofCounters

let networkInterface pn =
  match tryNetworkInterface pn with
  | None ->
    failwith "The performance counter category 'Network Interface' wasn't available on this machine"

  | Some metric ->
    metric

let appMetrics _ : Job<Metric> =
  ofCounters (appCounters ())

let systemMetrics _ : Job<Metric> =
  ofCounters (systemCounters ())