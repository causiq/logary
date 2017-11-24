module Logary.Metrics.WinPerfCounters

open Hopac
open Logary
open Logary.Metrics.WinPerfCounter
open Logary.Metrics.AllWinPerfCounters

let ofPerfCounters counters =
  counters
  |> Array.map toWindowsCounter
  |> Array.filter Option.isSome
  |> Array.map Option.get

let private forInstances instancesFactory unitsAndCounters =
  let instances : string [] = instancesFactory ()
  unitsAndCounters |> Array.map (fun (units : Units, create : string [] -> WinPerfCounter) ->
    units, create instances)

let private forInstance instanceFactory unitsAndCounters =
  forInstances (fun () -> [| instanceFactory () |]) unitsAndCounters

let systemCounters () =
  Array.concat [
    [| Div (Other "ctxsw", Seconds), System.``Context Switches/sec``
       Other "threads",       System.``Processor Queue Length`` |]

    [| Percent,               Processor.``% Processor Time``
       Percent,               Processor.``% User Time``
       Percent,               Processor.``% Interrupt Time``
       Percent,               Processor.``% Privileged Time``
       Div (Scalar, Seconds), Processor.``Interrupts/sec``
       Percent,               Processor.``% Idle Time`` 
    |] |> Array.map (fun (units, f) -> units, f (Processor.instances()))

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
    [| Div (Seconds, Other "read"), LogicalDisk.``Avg. Disk sec/Read``
       Div (Seconds, Other "write"), LogicalDisk.``Avg. Disk sec/Write``
       Scalar,                LogicalDisk.``Avg. Disk Read Queue Length``
       Scalar,                LogicalDisk.``Avg. Disk Write Queue Length``
       Div (Other "reads", Seconds), LogicalDisk.``Disk Reads/sec``
       Div (Other "reads", Seconds), LogicalDisk.``Disk Writes/sec``
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
  ]
  |> Array.map (fun (units, wpc) -> wpc |> WinPerfCounter.setUnit units)
  |> ofPerfCounters

/// https://www.codeproject.com/articles/42721/best-practices-no-detecting-net-application-memo
/// https://stackoverflow.com/questions/13473761/perfmon-counters-to-check-memory-leak
let appCounters () =
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
       Bytes,                 "# Bytes in all Heaps"
    |] |> Array.map (fun (units, counter) -> units, clrMem counter)

    [| Div (Scalar, Seconds), "# of Exceps Thrown / Sec"
       Div (Scalar, Seconds), "# of Filters / Sec"
       Div (Scalar, Seconds), "# of Finallys / Sec"
       Div (Scalar, Seconds), "Throw to Catch Depth / Sec"
    |] |> Array.map (fun (units, counter) -> units, clrExn counter)

    [| Other "threads",       "# of current logical Threads"
       Other "threads",       "# of current physical Threads"
       Div (Scalar, Seconds), "Contention Rate / Sec"
       Div (Scalar, Seconds), "Queue Length / sec"
    |] |> Array.map (fun (units, counter) -> units, clrLocks counter)
  ]
  |> forInstance (Helpers.pidInstance >> Option.get)
  |> Array.map (fun (units, wpc) -> wpc |> WinPerfCounter.setUnit units)
  |> ofPerfCounters

let byCategory pcc =
  Category.create pcc
  // gets all the instances for this performance counter category
  |> Option.map (fun pcc -> pcc, Category.instances pcc)
  // transform the list of instances into a list
  |> Option.map (fun (pcc, instances) -> WinPerfCounter.list pcc instances)


let ofCounters (counters : WinPerfCounterInstance []) =
  Ticker.create counters (fun counters item -> counters) (fun counters -> counters, counters |> Array.map Helpers.toValue)

/// The "GPU" category is installed by the nVIDIA drivers
let tryGPUMetric _ =
  byCategory "GPU" |> Option.map ofCounters

let gpuMetrics _ =
  match tryGPUMetric () with
  | None ->
    failwith "No GPUs configured (or drivers thereof) on this machine"
  | Some metric ->
    metric

let tryNVidiaGPUMetric _  =
  byCategory "NVIDIA GPU" |> Option.map ofCounters
  
let nvidiaMetrics pn =
  match tryNVidiaGPUMetric () with
  | None ->
    failwith "No nVIDIA GPUs configured (or drivers thereof) on this machine"

  | Some metric ->
    metric

let tryCpuInformation _  =
  byCategory "Processor Information" |> Option.map ofCounters

let cpuInformation _ =
  match tryCpuInformation () with
  | None ->
    failwith "The performance counter category 'Processor Information' wasn't available on this machine"

  | Some metric ->
    metric

let tryNetworkInterface _  =
  byCategory "Network Interface" |> Option.map ofCounters

let networkInterface pn =
  match tryNetworkInterface pn with
  | None ->
    failwith "The performance counter category 'Network Interface' wasn't available on this machine"

  | Some metric ->
    metric

let appMetrics (_ : PointName) =
  ofCounters (appCounters ())

[<CompiledName "AppMetric">]
let appMetric category counter instances=
  fun (_ : PointName) ->
    printfn "Creating app metric %s %s" category counter
    WinPerfCounter.create(category, counter, instances)
    |> Array.singleton
    |> ofPerfCounters
    |> ofCounters

[<CompiledName "AppMetricF">]
let appMetricF category counter instances =
  let fn = appMetric category counter instances
  System.Func<_, _>(fn)

let systemMetrics (_ : PointName) =
  ofCounters (systemCounters ())