namespace Hopac

open System
open System.Diagnostics
open System.Threading
open System.Threading.Tasks
open Hopac
open Hopac.Infixes
open Logary
open Logary.Internals

module Alt =

  let timeFun onComplete onNack (xA: Alt<'a>) =
    Alt.withNackJob (fun nack ->
      let ts = MonotonicClock.getTimestamp ()
      let start = Stopwatch.getTimestamp ()
      let markNack =
        nack |> Alt.afterFun (fun () ->
        let now = Stopwatch.getTimestamp ()
        onNack ts (Gauge.ofStopwatchTicks (now - start)))
      let altCommit =
        xA |> Alt.afterFun (fun x ->
        let now = Stopwatch.getTimestamp ()
        onComplete ts (Gauge.ofStopwatchTicks (now - start))
        x)
      Job.start markNack >>-. altCommit)

  let timeJob (onComplete: EpochNanoSeconds -> Gauge -> _) (onNack: EpochNanoSeconds -> Gauge -> _) (xA: Alt<'a>) =
    Alt.withNackJob (fun nack ->
      let ts = MonotonicClock.getTimestamp ()
      let start = Stopwatch.getTimestamp ()
      let markNack =
        nack |> Alt.afterJob (fun () ->
        let now = Stopwatch.getTimestamp ()
        onNack ts (Gauge.ofStopwatchTicks (now - start)))
      let altCommit =
        xA |> Alt.afterJob (fun x ->
        let now = Stopwatch.getTimestamp ()
        onComplete ts (Gauge.ofStopwatchTicks (now - start))
        >>-. x)
      Job.start markNack >>-. altCommit)

  let private disposable = { new IDisposable with member _.Dispose() = () }
  let inline private subscribe (ct: CancellationToken) (f: unit -> unit) =
    match ct with
    | _ when ct = CancellationToken.None ->
      disposable
    | ct ->
      upcast ct.Register (Action f)

  let toTask (ct: CancellationToken) (xA: Alt<'res>): Task<'res> =
    // attach to parent because placing in buffer should be quick in the normal case
    let tcs = TaskCompletionSource<'res>(TaskCreationOptions.AttachedToParent) // alloc TCS
    let nack = IVar () // alloc
    let sub = subscribe ct (fun () -> start (IVar.fill nack ())) // only alloc IVar if ct <> None
    start (
      Alt.tryFinallyFun (
        Alt.choose [
          Alt.tryIn xA
                    (fun res -> Job.thunk (fun () -> tcs.SetResult res))
                    (fun ex -> Job.thunk (fun () -> tcs.SetException ex))
          nack |> Alt.afterFun tcs.SetCanceled // |> Alt.afterFun (fun () -> printfn "Cancelled")
        ]
      ) sub.Dispose
    )
    // alloc on access?
    tcs.Task

  let internal toTasks bufferCt promiseCt (xAP: Alt<Promise<unit>>): Task<Task> =
    xAP
    |> Alt.afterFun (fun prom -> toTask promiseCt prom :> Task)
    |> toTask bufferCt

