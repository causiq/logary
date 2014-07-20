module Logary.Metrics.WinPerfCounters

open FSharp.Actor

open Logary
open Logary.Measure
open Logary.Metric

open Logary.WinPerfCounter

/// Configuration for the WinPerfCounters probe
type WinPerfCounterConf =
  { counters : PerfCounter list }

type WinPerfCountersMsg =
  | RegCounter of PerfCounter
  | UnregCounter of PerfCounter

type private WPCState =
  { lastValues : Map<PerfCounter, ``measure``> }
 
let private emptyState = { lastValues = [] }

let private calcPath (c : PerfCounter) =
  "TODO"

let private calcName (c : PerfCounter) =
  "TODO"

// TODO: implement things from TO_THINK_ABOUT

let private loop (conf : WinPerfCounterConf) (inbox : IActor<_>) =
  let rec loop state = async {
    let! msg, _ = inbox.Receive()
    match msg with
    | GetValue (datapoints, replChan) ->
      // what are the values for the requested data points?
      replChan.Reply(datapoints |> List.map (fun dp -> dp, Measure.empty))
      return! loop state
    | GetDataPoints replChan ->
      // what data points does this probe support?
      replChan.Reply [ DP "min"; DP "mean"; DP "99th percentile" ]
      return! loop state
    | Update msr ->
      // update the probe with the given measure
      return! loop state
    | Sample ->
      // read data from external sources and update state
      return! loop state
    | Shutdown ->
      return! shutdown state
    | Reset ->
      return! loop state
    }

  and shutdown state = async.Return ()

  loop emptyState

let create conf = MetricUtils.stdNamedMetric Probe (loop conf)