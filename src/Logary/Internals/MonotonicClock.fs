namespace Logary.Internals

open System
open System.Diagnostics
open System.Threading
open FSharp.Core.Operators.Checked
open NodaTime
open Hopac
open Logary

type private MonotonicClockState =
  { start: EpochNanoSeconds; sw: Stopwatch }

  static member create(maxSeen) =
    let now = DateTimeOffset.UtcNow.asTimestamp
    let sw = Stopwatch.StartNew()
    { start = max maxSeen now; sw = sw }

/// A monotonic clock implementation that always increases (per process) by at least one nanosecond
/// per call to `getTimestamp`.
///
/// It also tries to keep itself in sync with the global time by checking with DateTime.UtcNow every
/// other second. If the time has deviated, resets its internals, but still keeps the `maxSeen` value
/// at least what it was before the reset.
///
/// Read this for reference: https://stackoverflow.com/questions/1416139/how-to-get-timestamp-of-tick-precision-in-net-c
[<Sealed; AbstractClass>]
type MonotonicClock private () =
  // Global state (all values are in nanoseconds):
  static let mutable state = MonotonicClockState.create 0L
  // All writes to this static field must write monotonically increasing values
  static let mutable maxSeen = state.start

  // Reset logic
  static let reset () =
    let mutable c = true
    while c do
      let oldState = state
      let newState = MonotonicClockState.create(maxSeen)
      // 1: location1: The destination, whose value is compared by reference with comparand and possibly replaced.
      // 2: value: The value that replaces the destination value if the comparison by reference results in equality.
      // 3: comparand: The value that is compared by reference to the value at location1.
      // ret: The original value in location1.
      // In short; if oldState = state, do state <- newState and stop iteration
      c <- not <|
        obj.ReferenceEquals(
          Interlocked.CompareExchange(&state, newState, oldState),
          oldState)

    // we might have a race here too, so use `max`
    maxSeen <- max (maxSeen + 1L) state.start

  // Constants for this process (Stopwatch.Frequency is per process):
  static let allowedDelta = 20_000_000L // 20 ms
  static let nsPerTimerTick = 1_000_000_000L / Stopwatch.Frequency

  // Sync logic + Hopac server
  static let rec sync () =
    timeOutMillis 2000 |> Job.bind setter

  and setter () =
    let dtNow = DateTimeOffset.UtcNow.asTimestamp
    let swNow = state.start + state.sw.ElapsedTicks * nsPerTimerTick
    if abs (dtNow - swNow) > allowedDelta then
      eprintfn "MonotonicClock is resetting; dtNow=%i, swNow=%i, dt=%i" dtNow swNow (abs (dtNow - swNow))
      reset ()
    sync ()

  // We start a global Hopac server that ensures we don't drift more than 20ms from the actual time.
  static do server (sync ())

  static let inst =
    lazy ({ new IClock with
              member x.GetCurrentInstant() =
                Instant.ofEpoch (MonotonicClock.getTimestamp())
          })

  static member instance = inst.Value

  static member getTimestamp() =
    let swNow = state.start + state.sw.ElapsedTicks * nsPerTimerTick
    let now = max swNow (maxSeen + 1L)
    maxSeen <- now
    now
