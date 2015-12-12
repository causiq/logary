  /// A metric that just ignores all calls best it can
module Logary.Metrics.Noop

open Hopac
open Hopac.Infixes

open Logary
open Logary.Internals
open Logary.Metric

type NoopConf =
  { isHappy : bool }

let empty = { isHappy = true }

module private Impl =

  type NoopState =
    { calls : bigint }

  let loop (conf : NoopConf) (ri : RuntimeInfo) (ch: MetricInstance) =

    let handleRequest state msg = job {
      match msg with
      | GetValue (dps, resultCh) ->
        match dps with
        | pn :: _ when pn = PointName.ofSingle "calls" ->
          do! Ch.give resultCh [ Message.metric pn (BigInt state.calls) ]

        | _ ->
          do! Ch.give resultCh []
        return { state with calls = state.calls + 1I }

      | Sample ->
        return state

      | Reset ->
        return { state with calls = 0I }

      | Shutdown ack ->
        do! IVar.fill ack Ack
        return state
    }

    let handleUpdate state msg =
      match msg.value with
      | Derived (BigInt i, _)
        -> {state with calls = i}

      | _
        -> {state with calls = state.calls + 1I}

      |> Job.result

    Job.iterateServer { calls = 0I } <| fun state ->
      Alt.choose [
        Ch.take ch.requestCh ^=> handleRequest state
        Ch.take ch.updateCh  ^=> handleUpdate state
        Ch.give ch.dpNameCh  [ PointName.ofSingle "calls" ] ^->. state
      ]

/// Create a new Noop metric that doesn't do very much
let create conf =
  MetricUtils.stdNamedMetric Metric (Impl.loop conf)