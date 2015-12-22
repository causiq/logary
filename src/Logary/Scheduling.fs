/// A scheduling actor that can call `Sample` on the metric/probe/health check.
module Logary.Internals.Scheduling
#nowarn "64"

// creds to Dave Thomas for his F# snippet
open System.Threading

open Hopac
open Hopac.Infixes

open NodaTime

type Cancellation = private { cancelled: IVar<unit> }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Cancellation =
  let create () =
    { cancelled = IVar.Now.create () }

  let isCancelled cancellation =
    IVar.Now.isFull cancellation.cancelled

  let cancel cancellation = job {
    do! IVar.tryFill cancellation.cancelled ()
  }

type ScheduleMsg =
  | Schedule of (obj -> Job<unit>) * obj * Duration * Duration * Cancellation
  | ScheduleOnce of (obj -> Job<unit>) * obj * Duration * Cancellation

module private Impl =

  let ms (d: Duration) =
    d.ToTimeSpan().TotalMilliseconds |> int

  let scheduleOnce delay msg (receiver : _ -> Job<unit>) cts = job {
    do! timeOutMillis delay

    if not <| Cancellation.isCancelled cts then
      do! receiver msg
  }

  let scheduleMany initialDelay msg (receiver : _ -> Job<unit>) delayBetween cts = Job.delay <| fun () ->
    let rec loop time cts = job {
      do! timeOutMillis time

      if not <| Cancellation.isCancelled cts then
        do! receiver msg
        return! loop delayBetween cts
    }

    loop initialDelay cts

  let loop (ch : Ch<ScheduleMsg>) =
    let rec loop () = job {
      let! msg = Ch.take ch
      match msg with
      | Schedule (receiver, msg : 'a, initialDelay, delayBetween, cts) ->
        do! Job.start (scheduleMany (ms initialDelay) msg receiver (ms delayBetween) cts)
        return! loop ()

      | ScheduleOnce (receiver, msg : 'a, delay, cts) ->
        do! Job.start (scheduleOnce (ms delay) msg receiver cts)
        return! loop ()
    }

    loop ()

/// Creates a new scheduler job
let create () =
  let ch = Ch ()
  Job.Global.server (Impl.loop ch)
  ch

/// Schedules a message to be sent to the receiver after the initialDelay.
/// If delayBetween is specified then the message is sent reoccuringly at the
/// delay between interval.
let schedule scheduler (receiver : 'a -> Job<unit>) (msg : 'a) initialDelay (delayBetween: _ option) =
  let cts = Cancellation.create ()

  let message =
    match delayBetween with
    | Some x ->
      Schedule (unbox >> receiver, msg, initialDelay, x, cts)

    | None ->
      ScheduleOnce (unbox >> receiver, unbox msg, initialDelay, cts)

  Ch.send scheduler message |> Job.map (fun _ -> cts)
