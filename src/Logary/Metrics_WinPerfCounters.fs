module Logary.Metrics.WinPerfCounters

open System.Diagnostics

open FSharp.Actor

open Logary
open Logary.Measure
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

  /// nice metrics about CPU time
  let cpuTime =
    [ "% Processor Time"
      "% User Time"
      "% Interrupt Time"
      "% Processor Time" ]
    |> List.map (fun counter ->
      { category = "Processor"
        counter  = counter
        instance = Some AllInstances })
    |> fun cs -> { initCounters = cs }

module private Impl =

  type WPCState =
    { lastValues : Map<DP, PC * ``measure``> }

  /// A unified naming scheme for the names of performance counters
  module Naming =
    let toDP (c : PerfCounter) =
      let fstr instance =
        match instance with
        | None -> sprintf "%s|%s" c.category c.counter
        | Some inst -> sprintf "%s|%s|%s" c.category c.counter inst
      fstr c.instance |> DP

    let toCounter (DP dp) =
      match dp.Split '|' with
      | ss when ss.Length = 2 -> { category = ss.[0]; counter = ss.[1]; instance = None }
      | ss -> { category = ss.[0]; counter = ss.[1]; instance = Some ss.[2] }

  let tryGetPc (lastValues : Map<DP, PC * ``measure``>) dp =
    lastValues
    |> Map.tryFind dp
    |> Option.map fst
    |> function
    | None -> mkPc (Naming.toCounter dp)
    | x -> x

  let pcNextValue (DP dp) (pc : PC) =
    Measure.create dp (nextValue pc)

  // TODO: consider adding in a reservoir to hold the values? Or should this go
  // somewhere else like in the targets or in the registry or somewhere else?

  // TODO: consider this metric/probe for each performance counter instead
  // of having it for a list of PerfCounters?

  // TODO: hook up to health checks, example with CPU EWMA impl on CPU usage?

  // in the first incarnation, this actor doesn't do a fan-out, so beware of slow
  // perf counters...
  let loop (conf : WinPerfCounterConf) (ri : RuntimeInfo) (inbox : IActor<_>) =
    let rec init (pcs : PerfCounter list) =
      loop { lastValues = conf.initCounters
                          |> List.map WinPerfCounter.mkPc
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
            | dp, Some (pc, msr) -> dp, msr
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
        let key = DP msr.m_path
        match tryGetPc state.lastValues key with
        | None ->
          // perf counter key cannot be made a performance counter out of,
          // so continue with our business
          return! loop state
        | Some pc ->
          // otherwise update the state with the new measurement
          let state' = { state with lastValues = state.lastValues |> Map.put key (pc, msr) }
          return! loop state'
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