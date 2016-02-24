module Logary.Metrics.WinPerfCounters

open System.Diagnostics

open Hopac

open Logary
open Logary.Metric
open Logary.Internals

open Logary.WinPerfCounter

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

module internal Impl =

  type WPCState =
    { lastValues : Map<PointName, PointName * Value> }

  /// A unified naming scheme for the names of performance counters
  module Naming =
    let toDP (c : PerfCounter) =
      let fstr instance =
        match instance with
        | NotApplicable -> [ c.category; c.counter ]
        | Instance inst -> [ c.category; c.counter; inst ]
      PointName (fstr c.instance)

    let toCounter (PointName dp) =
      match dp with
      | [ category; counter ] ->
        { category = category; counter = counter; instance = NotApplicable }
      | [ category; counter; instance ] ->
        { category = category; counter = counter; instance = Instance instance }
      | _ -> failwithf "unknown performance counter name: %A" dp

  let tryGetPc (lastValues : Map<PointName, PC * Value>) dp =
    lastValues
    |> Map.tryFind dp
    |> Option.map fst
    |> function
    | None -> toPC (Naming.toCounter dp)
    | x -> x

  let pcNextValue dp (pc : PC) =
    Float (decimal (nextValue pc))

  // TODO: consider adding in a reservoir to hold the values? Or should this go
  // somewhere else like in the targets or in the registry or somewhere else?

  // TODO: consider this metric/probe for each performance counter instead
  // of having it for a list of PerfCounters?

  // TODO: hook up to health checks, example with CPU EWMA impl on CPU usage?

  // in the first incarnation, this actor doesn't do a fan-out, so beware of slow
  // perf counters...
  let loop (conf : WinPerfCounterConf) (ri : RuntimeInfo) (inbox : IActor<_>) =
    let rec init (pcs : PerfCounter list) =
      loop { lastValues = pcs
                          |> List.map WinPerfCounter.toPC
                          |> List.zip (conf.initCounters |> List.map Naming.toDP)
                          |> List.filter (Option.isSome << snd)
                          |> List.map (fun (dp, pc) -> dp, (pc.Value, Measure.empty))
                          |> Map.ofList }

    and loop (state : WPCState) = async {
      let! msg, _ = inbox.Receive()
      match msg with
      | GetValue (datapoints, replChan) ->
        // what are the values for the requested data points?
        let ret =
          datapoints
          |> List.map (flip Map.tryFind state.lastValues)
          |> List.zip datapoints
          |> List.filter (Option.isSome << snd) // the datapoints that had values
          |> List.map (function
            | dp, Some (pc, msr) -> msr
            | dp, None -> failwith "isSome above says this won't happen")
        replChan.Reply ret
        return! loop state
      | GetDataPoints replChan ->
        // what are the DPs I support?
        let dps = state.lastValues |> Map.fold (fun acc key _ -> key :: acc) []
        replChan.Reply dps
        return! loop state
      | Update msr ->
        // update specific DP
        match tryGetPc state.lastValues msr.m_path with
        | None ->
          // perf counter key cannot be made a performance counter out of,
          // so continue with our business
          return! loop state
        | Some pc ->
          // otherwise update the state with the new measurement
          return! loop
            { state with
                lastValues = state.lastValues |> Map.put msr.m_path (pc, msr) }
      | Sample ->
        // read data from external sources and update state
        let update key (pc, _) = pc, pcNextValue key pc
        let state' = { state with lastValues = state.lastValues |> Map.map update }
        return! loop state'
      | Shutdown replChan ->
        return shutdown state replChan
      | Reset ->
        return! reset state
      }

    and reset state =
      // return to init state after disposing all performance countrs
      state.lastValues |> Map.iter (fun dp (pc, _) -> pc.Dispose())
      init conf.initCounters

    and shutdown state replChan =
      // dispose all perf counters and exit
      state.lastValues |> Map.iter (fun dp (pc, _) -> pc.Dispose())
      replChan.Reply Ack

    // initialise all perf counters
    init conf.initCounters

let create conf = MetricUtils.stdNamedMetric Probe (Impl.loop conf)
