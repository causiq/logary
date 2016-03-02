module Logary.Metrics.WinPerfCounters

open System.Diagnostics

open Hopac

open Logary
open Logary.Metric
open Logary.Internals

open Logary.Metrics.WinPerfCounter

/// Configuration for the WinPerfCounters probe
type WinPerfCounterConf =
  { initCounters : PerfCounter list }

type WinPerfCountersMsg =
  | Register of PerfCounter
  | Unregister of PerfCounter

module Common =
  open WinPerfCounters

  let system =
    [ System.``Context Switches/sec``
      System.``Processor Queue Length`` ]

  let dotnet instance =
    [ ``_NET CLR Exceptions``.``# of Exceps Thrown / sec`` instance ]

  let cpuTime =
    [ Processor.``% Processor Time``
      Processor.``% User Time``
      Processor.``Interrupts/sec``
      Processor.``% Idle Time`` ]
    |> List.map (fun f -> f (Instance WinPerfCounter.KnownInstances._Total))

  let cpuTimeConf = { initCounters = cpuTime }

  /// see aspNetRecommended
  let aspNetGlobal =
    [ ASP_NET.``Application Restarts``
      ASP_NET.``Requests Queued``
      ASP_NET.``Worker Process Restarts`` ]

  /// see aspNetRecommended
  let aspNet instance =
    [ ``ASP_NET Applications``.``Errors Total`` instance
      ``ASP_NET Applications``.``Requests/Sec`` instance ]

  /// Useful ASP.Net counters, from
  /// http://technet.microsoft.com/en-us/library/cc778343%28v=ws.10%29.aspx
  ///
  /// You can either pass the app domain instance process name, or you could use
  /// WinPerfCounter.pidInstance () to get your own instance name. If you have
  /// multiple processes with the same name executing, you will need to
  /// discriminate by name.
  ///
  /// TODO: test this further; in two ways; as a ASP.Net metric, and as a server
  /// health check that runs globally and uses the KnownInstances._Total
  /// instance.
  let aspNetRecommended instance =
    aspNetGlobal
    @ aspNet instance
    @ cpuTime
    @ system
    @ dotnet instance

  let aspNetRecommendedConf instance =
    { initCounters = aspNetRecommended instance }

  type WPCState =
    { lastValues : Map<PointName, PointName * Value> }

  /// A unified naming scheme for the names of performance counters
  module Naming =

    let toCounter (PointName dp) =
      match dp with
      | [ category; counter ] ->
        { category = category; counter = counter; instance = NotApplicable }

      | [ category; counter; instance ] ->
        { category = category; counter = counter; instance = Instance instance }

      | _ ->
        failwithf "unknown performance counter name: %A" dp

  let tryGetPc (lastValues : Map<PointName, PC * Value>) dp =
    lastValues
    |> Map.tryFind dp
    |> Option.map fst
    |> function
    | None -> toPC (Naming.toCounter dp)
    | x -> x

  let pcNextValue dp (pc : PC) =
    Float (nextValue pc)

