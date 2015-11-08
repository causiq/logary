/// A scheduling actor that can call `Sample` on the metric/probe/health check.
module Logary.Internals.Scheduling
#nowarn "64"

// creds to Dave Thomas for his F# snippet
open System.Threading

open FSharp.Actor

open Hopac
open Hopac.Job.Infixes

open NodaTime

type NamedJob<'a> = NamedJob of name: string * Job<'a>

type ScheduleMsg =
  | Schedule of (obj -> unit) * obj * Duration * Duration * CancellationTokenSource ReplyChannel
  | ScheduleOnce of (obj -> unit) * obj * Duration * CancellationTokenSource ReplyChannel

type Cancellation = {
  cancelCh: Ch<unit>
  getCh: Ch<bool>
}

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Cancellation =
  let create () =
    let c = { cancelCh = Ch.Now.create ()
              getCh    = Ch.Now.create () }

    Job.iterateServer false (fun x ->
      Alt.choose [Alt.map (fun _ -> false) (Ch.take c.cancelCh)
                  Alt.map (fun _ -> x)     (Ch.give c.getCh x)])
    |> Job.start |> ignore

    c

  let get cancellation = job {
    return! Ch.take cancellation.getCh
  }

  let cancel cancellation = job {
    do! Ch.give cancellation.cancelCh ()
  }

type HopacScheduleMsg =
  | HopacSchedule of (obj -> unit) * obj * Duration * Duration * Cancellation
  | HopacScheduleOnce of (obj -> unit) * obj * Duration * Cancellation

module private HopacImpl =

  let ms (d: Duration) =
    d.ToTimeSpan().TotalMilliseconds |> int

  let schedeluOnce delay msg receiver cts = job {
    do! timeOutMillis delay
    let! cancelled = Cancellation.get cts

    if not cancelled then
      receiver msg
  }

  let scheduleMany initialDelay msg receiver delayBetween cts = Job.delay <| fun () ->
    let rec loop time cts = job {
      do! timeOutMillis time
      let! cancelled = Cancellation.get cts

      if not cancelled then
        receiver msg
        return! loop delayBetween cts
    }

    loop initialDelay cts

  let loop (ch : Ch<HopacScheduleMsg>) =
    let rec loop () = job {
      let! msg = Ch.take ch
      match msg with
      | HopacSchedule (receiver, msg : 'a, initialDelay, delayBetween, cts) ->
        Job.start (scheduleMany (ms initialDelay) msg receiver (ms delayBetween) cts) |> ignore
        return! loop ()
      | HopacScheduleOnce (receiver, msg : 'a, delay, cts) ->
        Job.start (schedeluOnce (ms delay) msg receiver cts) |> ignore
        return! loop ()
    }

    loop ()

let hopacCreate () =
  let ch = Ch.Now.create ()
  Job.start (HopacImpl.loop ch) |> ignore
  mb

let hopacSchedule scheduler (receiver : 'a -> unit) (msg : 'a) initialDelay (delayBetween: _ option) =
  let cts = Cancellation.create ()
  let message =
    match delayBetween with
    | Some x ->
      HopacSchedule (receiver, msg, initialDelay, x, cts)
    | None ->
      HopacScheduleOnce (receiver, msg, initialDelay, cts)

  Ch.send scheduler message |> Job.start |> ignore
  cts

module private Impl =

  let ms (d : Duration) =
    d.ToTimeSpan().TotalMilliseconds |> int

  let scheduleOnce (delay : Duration) msg receiver (cts: CancellationTokenSource) = async {
    do! Async.Sleep (ms delay)
    if cts.IsCancellationRequested then
      cts.Dispose ()
    else
      receiver msg
    }

  let scheduleMany initialDelay msg receiver delayBetween cts =
    let rec loop time (cts: CancellationTokenSource) = async {
      do! Async.Sleep time
      if cts.IsCancellationRequested then
        cts.Dispose ()
      else
        receiver msg
      return! loop delayBetween cts
    }
    loop initialDelay cts

  let loop (inbox : IActor<_>) =
    let rec loop () = async {
      let! msg, _ = inbox.Receive ()
      let cts = new CancellationTokenSource()
      match msg with
      | Schedule (receiver, msg : 'a, initialDelay, delayBetween, replyChan) ->
        Async.StartImmediate (scheduleMany (ms initialDelay) msg receiver (ms delayBetween) cts)
        replyChan.Reply cts
        return! loop ()
      | ScheduleOnce (receiver, msg:'a, delay, replyChan) ->
        Async.StartImmediate (scheduleOnce delay msg receiver cts)
        replyChan.Reply cts
        return! loop ()
    }
    loop ()

/// Create a new scheduler actor
let create () =
  Actor.spawn (Ns.create "scheduler") Impl.loop

/// Schedules a message to be sent to the receiver after the initialDelay.
/// If delayBetween is specified then the message is sent reoccuringly at the
/// delay between interval.
let schedule scheduler (receiver : 'a -> unit) (msg : 'a) initialDelay (delayBetween : _ option) =
  // this is specific to scheduling/sending to actors:
  let swallowInvalidState f x =
    try
      f x
    with
    | Actor.ActorInvalidStatus _ -> ()

  let buildMessage replyChan =
    match delayBetween with
    | Some x ->
      Schedule (unbox >> swallowInvalidState(receiver), msg, initialDelay, x, replyChan)
    | _ ->
      ScheduleOnce (unbox >> receiver, msg, initialDelay, replyChan)
  scheduler |> Actor.reqReply buildMessage Infinite
