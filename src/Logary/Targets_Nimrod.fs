// https://github.com/sbtourist/nimrod
module Logary.Targets.Nimrod

open FSharp.Actor
open NodaTime

open Logary
open Logary.Target
open Logary.Measure
open Logary.Internals
open Logary.Internals.Tcp
open Logary.Internals.Date

/// To configure the Nimrod target, just point at some other target, and the Nimrod
/// target will take care of formatting according to the correct
/// Nimrod semantics.
type NimrodConf = { target : TargetInstance }

let private actorLoop conf metadata =
  (fun (inbox : IActor<_>) ->
    let rec loop () = async { return! running () }
    and running () = async {
        let! msg, mopts = inbox.Receive()
        match msg with
        | Measure m ->
          let ts = m.m_timestamp.Ticks / NodaConstants.TicksPerSecond
          let m' = sprintf "[nimrod][%i][%s][%s][%s]" ts (m.ToString()) m.m_path (getValueStr m)
          { message       = m'
            level         = m.m_level
            data          = Map.empty
            path          = m.m_path
            tags          = []
            ``exception`` = None
            timestamp     = m.m_timestamp }
          |> sendLogLine conf.target
          return! loop ()
        | Log l ->
          return! running ()
        | Flush chan ->
          chan.Reply Ack
          return! running ()
        | Shutdown ackChan ->
          ackChan.Reply Ack
          return! shutdown () }
    and shutdown () = async { return () }
    loop ())

let create conf = TargetUtils.stdNamedTarget (actorLoop conf)

[<CompiledName("Create")>]
let create' (conf, name)  = create conf name
