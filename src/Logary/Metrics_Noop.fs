/// A metric that just ignores all calls best it can
module Logary.Metrics.Noop

open FSharp.Actor
open Hopac
open Hopac.Alt.Infixes

open Logary
open Logary.Internals
open Logary.Metric
open Logary.DataModel

type NoopConf =
  { isHappy : bool }

let empty = { isHappy = true }

module private HopacImpl =

  type NoopState =
    { calls : bigint }

  let loop (conf : NoopConf) (ri : RuntimeInfo) (ch: MetricCh) =

    let handleRequest state msg = job {
      match msg with
      | HopacGetValue dps ->
        match dps with
        | pn :: _ when pn = [ "calls" ] ->
          do! Ch.give ch.dpValueCh [ Message.metric pn (BigInt state.calls) ]
        | _ ->
          do! Ch.give ch.dpValueCh []
        return {state with calls = state.calls + 1I}
      | HopacSample ->
        return state
      | HopacReset ->
        return {state with calls = 0I}
    }

    let handleUpdate state msg =
      match msg.value with
      | Derived (BigInt i, _)
        -> {state with calls = i}
      | _
        -> {state with calls = state.calls + 1I}
      |> Job.result

    Job.iterateServer {calls = 0I} <| fun state ->
      Alt.choose [Ch.take ch.requestCh >>=? handleRequest state
                  Ch.take ch.updateCh  >>=? handleUpdate state
                  Ch.give ch.dpNameCh  [ [ "calls"] ] >>%? state
                  Ch.give ch.shutdownCh Ack >>%? state]

module private Impl =

  type NoopState =
    { calls : bigint }

  let loop (conf : NoopConf) (ri : RuntimeInfo) (inbox : IActor<_>) =
    let rec loop state = async {
      let! msg, _ = inbox.Receive()
      match msg with
      | GetValue (dps, replChan) ->
        match dps with
        | pn :: _ when pn = [ "calls" ] ->
          replChan.Reply [ Message.metric pn (BigInt state.calls) ]
        | _ ->
          replChan.Reply []
        return! loop { calls = state.calls + 1I }
      | GetDataPoints replChan ->
        replChan.Reply [ [ "calls" ] ]
        return! loop { calls = state.calls + 1I }
      | Update msr ->
        match msr.value with
        | Derived (BigInt i, _) ->
          return! loop { calls = i }
        | _ ->
          return! loop state
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
let create conf      = MetricUtils.stdNamedMetric      Metric (Impl.loop conf)
let hopacCreate conf = MetricUtils.hopacStdNamedMetric Metric (HopacImpl.loop conf)

/// C# interop: Create a new Noop metric that doesn't do very much
[<CompiledName "Create">]
let create' (conf, name) =
  create conf name
