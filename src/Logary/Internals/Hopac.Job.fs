namespace Hopac

open System.Diagnostics
open System.Runtime.CompilerServices
open System.Threading.Tasks
open Hopac
open Hopac.Infixes
open Logary
open Logary.Internals

module Job =
  let time onComplete xJ =
    Job.delay (fun () ->
      let ts = MonotonicClock.getTimestamp()
      let start = Stopwatch.getTimestamp()
      xJ >>- fun x ->
      let now = Stopwatch.getTimestamp()
      onComplete ts (Gauge.ofStopwatchTicks (now - start))
      x)

  let timeJob onCompleteJ xJ =
    Job.delay (fun () ->
      let ts = MonotonicClock.getTimestamp()
      let start = Stopwatch.getTimestamp()
      xJ >>= fun x ->
      let now = Stopwatch.getTimestamp()
      onCompleteJ ts (Gauge.ofStopwatchTicks (now - start)) >>- fun () ->
      x)

[<AutoOpen>]
module JobEx =
  /// Functions callable by Logary.CSharp.
  [<Extension>]
  type Job =
    [<Extension>]
    static member ToTask<'a> (xJ: Job<'a>): Task<'a> =
      let tcs = TaskCompletionSource<'a>()
      xJ
        |> Job.catch
        |> Job.map (function Choice1Of2 res -> tcs.SetResult res | Choice2Of2 e -> tcs.SetException e)
        |> start
      tcs.Task
