/// A metric that just ignores all calls best it can
module Logary.Metrics.Noop

open FSharp.Actor

open Logary
open Logary.Internals
open Logary.Metric

type NoopConf =
  { isHappy : bool }

let empty = { isHappy = true }

module private Impl =

  type NoopState =
    { calls : bigint }

  let loop (conf : NoopConf) (ri : RuntimeInfo) (inbox : IActor<_>) =
    let rec loop state = async {
      let! msg, _ = inbox.Receive()
      match msg with
      | GetValue (dps, replChan) ->
        match dps with
        | (DP h as dp) :: _ when h = [ "calls" ] ->
          replChan.Reply [ Measure.create dp (float state.calls) ] //of DP list * ReplyChannel<(DP * ``measure``) list>
        | _ ->
          replChan.Reply []
        return! loop { calls = state.calls + 1I }
      | GetDataPoints replChan ->
        replChan.Reply [ DP [ "calls" ] ]
        return! loop { calls = state.calls + 1I }
      | Update msr ->
        match msr.m_value with
        | F f -> return! loop { calls = bigint f }
        | L l   -> return! loop { calls = state.calls + 1I }
      | Sample ->
        return! loop state
      | Shutdown ackChan ->
        ackChan.Reply Ack
        return! loop state
      | Reset ->
        return! loop { calls = 0I }
    }
    loop { calls = 0I }

/// Create a new Noop metric that doesn't do very much
let create conf = MetricUtils.stdNamedMetric Metric (Impl.loop conf)

/// C# interop: Create a new Noop metric that doesn't do very much
[<CompiledName "Create">]
let create' (conf, name) =
  create conf name
