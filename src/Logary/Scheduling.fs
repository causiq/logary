/// A scheduling actor that can call `Sample` on the metric/probe/health check.
module Logary.Internals.Scheduling
#nowarn "64"

// creds to Dave Thomas for his F# snippet
open System.Threading

open Hopac
open Hopac.Infixes

open NodaTime

type NamedJob<'a> = NamedJob of name: string * Job<'a>

type Cancellation = private { cancelled: IVar<unit> }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Cancellation =
  let create () = { cancelled = IVar.Now.create () }

  let isCancelled cancellation = job {
    return! (IVar.read cancellation.cancelled ^->. true) <|> (Alt.always false)
  }

  let cancel cancellation = job {
    do! IVar.fill cancellation.cancelled ()
  }

type ScheduleMsg =
  | Schedule of (obj -> unit) * obj * Duration * Duration * Cancellation
  | ScheduleOnce of (obj -> unit) * obj * Duration * Cancellation

module private Impl =

  let ms (d: Duration) =
    d.ToTimeSpan().TotalMilliseconds |> int

  let schedeluOnce delay msg receiver cts = job {
    do! timeOutMillis delay
    let! cancelled = Cancellation.isCancelled cts

    if not cancelled then
      receiver msg
  }

  let scheduleMany initialDelay msg receiver delayBetween cts = Job.delay <| fun () ->
    let rec loop time cts = job {
      do! timeOutMillis time
      let! cancelled = Cancellation.isCancelled cts

      if not cancelled then
        receiver msg
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
        do! Job.start (schedeluOnce (ms delay) msg receiver cts)
        return! loop ()
    }

    loop ()

/// Creates a new scheduler job
let create () =
  let ch = Ch.Now.create ()
  Job.Global.start (Impl.loop ch)
  ch

/// Schedules a message to be sent to the receiver after the initialDelay.
/// If delayBetween is specified then the message is sent reoccuringly at the
/// delay between interval.
let schedule scheduler (receiver : 'a -> unit) (msg : 'a) initialDelay (delayBetween: _ option) =
  let cts = Cancellation.create ()

  let message =
    match delayBetween with
    | Some x ->
      Schedule (unbox >> receiver, msg, initialDelay, x, cts)
    | None ->
      ScheduleOnce (unbox >> receiver, unbox msg, initialDelay, cts)

  Ch.send scheduler message |> Job.Global.run
  cts
