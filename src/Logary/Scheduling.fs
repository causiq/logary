/// A scheduling actor that can call `Sample` on the metric/probe/health check.
module Logary.Internals.Scheduling

// creds to Dave Thomas for his F# snippet
open System.Threading
  
open FSharp.Actor

open NodaTime

type ScheduleMsg<'a> =
  | Schedule of ('a -> unit) * 'a * Duration * Duration * CancellationTokenSource ReplyChannel
  | ScheduleOnce of ('a -> unit) * 'a * Duration * CancellationTokenSource ReplyChannel

let private ms (d : Duration) =
  d.ToTimeSpan().TotalMilliseconds |> int

let private scheduleOnce (delay : Duration) msg receiver (cts: CancellationTokenSource) = async {
  do! Async.Sleep (ms delay)
  if cts.IsCancellationRequested then
    cts.Dispose ()
  else
    receiver msg
  }
  
let private scheduleMany initialDelay msg receiver delayBetween cts =
  let rec loop time (cts: CancellationTokenSource) = async {
    do! Async.Sleep time
    if cts.IsCancellationRequested then
      cts.Dispose ()
    else
      receiver msg
    return! loop delayBetween cts
  }
  loop initialDelay cts

let private schedulerLoop (inbox : IActor<_>) =
  let rec loop() = async {
    let! msg, _ = inbox.Receive ()
    let cs = new CancellationTokenSource()
    match msg with
    | Schedule (receiver, msg : 'a, initialDelay, delayBetween, replyChan) ->
      Async.StartImmediate (scheduleMany (ms initialDelay) msg receiver (ms delayBetween) cs)
      replyChan.Reply cs
      return! loop ()
    | ScheduleOnce (receiver, msg:'a, delay, replyChan) ->
      Async.StartImmediate (scheduleOnce delay msg receiver cs)
      replyChan.Reply cs
      return! loop ()
  }
  loop ()

let create () =
  Actor.spawn (Actor.Options.Create "logaryRoot/scheduler") schedulerLoop

/// Schedules a message to be sent to the receiver after the initialDelay.
/// If delayBetween is specified then the message is sent reoccuringly at the
/// delay between interval.
let schedule scheduler receiver msg initialDelay (delayBetween : _ option) =
  let buildMessage replyChan =
    match delayBetween with
    | Some x ->
      Schedule (receiver, msg, initialDelay, x, replyChan)
    | _ ->
      ScheduleOnce (receiver, msg, initialDelay, replyChan)
  scheduler |> Actor.reqReply buildMessage Infinite
