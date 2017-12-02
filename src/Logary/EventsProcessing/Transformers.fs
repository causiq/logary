namespace Logary.EventsProcessing.Transformers

module Snapshot =
  open System
  // TODO: when I need to read values multiple times:
  // memoized snapshot to avoid recalculation of values after reading

  type Snapshot =
    { values : int64 [] }

  let create unsorted =
    Array.sortInPlace unsorted
    { values = unsorted }

  let size s = s.values.Length

  let quantile s q =
    if q < 0. || q > 1. then
      invalidArg "q" "quantile is not in [0., 1.]"
    if size s = 0 then
      0.
    else
      let pos = q * float (size s + 1)
      match pos with
      | _ when pos < 1. ->
        float s.values.[0]
      | _ when pos >= float (size s) ->
        float s.values.[size s - 1]
      | _ ->
        let lower = s.values.[int pos - 1]
        let upper = s.values.[int pos]
        float lower + (pos - floor pos) * float (upper - lower)

  let median s = quantile s 0.5
  let percentile75th s = quantile s 0.75
  let percentile95th s = quantile s 0.95
  let percentile98th s = quantile s 0.98
  let percentile99th s = quantile s 0.99
  let percentile999th s = quantile s 0.999

  let values s = s.values
  let min s = if size s = 0 then 0L else Array.min s.values
  let max s = if size s = 0 then 0L else Array.max s.values

  let private meanAndSum s =
    if size s = 0 then 0., 0. else
    let mutable sum = 0.
    for x in s.values do
      sum <- sum + float x
    let mean = float sum / float s.values.Length
    mean, sum

  let mean = fst << meanAndSum

  let stdDev s =
    let size = size s
    if size = 0 then 0. else
    let mean = mean s
    let sum = s.values |> Array.sumBy (fun d -> Math.Pow(float d - mean, 2.))
    sqrt (sum / float (size - 1))

// starting off single-threaded
module Uniform =
  open System
  open System.Diagnostics.Contracts
  open Logary.Internals

  let private DefaultSize = 1028

  /// Mutable uniform distribution
  type UniformState =
    { count  : bigint
      values : int64 [] }

  [<Pure>]
  let create size =
    { count  = 0I
      values = Array.zeroCreate size }

  [<Pure>]
  let empty = create DefaultSize

  [<Pure>]
  let size r = int (min r.count (bigint r.values.Length))

  [<Pure>]
  let snapshot r =
    Snapshot.create (r.values |> Seq.take (size r) |> Array.ofSeq)

  // impure update the reservoir
  let update r value =
    let count' = r.count + 1I
    if count' <= bigint r.values.Length then
      r.values.[int count' - 1] <- value
    else
      let rnd = Rnd.nextInt' (min (int count') Int32.MaxValue)
      if rnd < r.values.Length then
        r.values.[rnd] <- value

    { r with count = r.count + 1I }

module SlidingWindow =
  open System

  let private DefaultSize = 1028

  type SlidingState =
    { count  : bigint
      values : int64 [] }

  let create size =
    { count  = 0I
      values = Array.zeroCreate size }

  let size r = int (min r.count (bigint r.values.Length))

  let snapshot r =
    Snapshot.create (r.values |> Seq.take (size r) |> Array.ofSeq)

  let update r value =
    let count' = r.count + 1I
    r.values.[int (r.count % (bigint r.values.Length))] <- value
    { r with count = count' }

/// The an exponentially weighted moving average that gets ticks every
/// period (a period is a duration between events), but can get
/// `update`s at any point between the ticks.
module ExpWeightedMovAvg =
  open NodaTime
  open Logary
  open Logary.Internals

  /// calculate the alpha coefficient from a number of minutes
  ///
  /// - `samplePeriod` is how long is between each tick
  /// - `alphaPeriod` is the duration the EWMA should be calculated over
  let alpha (samplePeriod : Duration) (alphaPeriod : Duration) =
    1. - exp (- (samplePeriod.TotalNanoseconds / alphaPeriod.TotalNanoseconds))


  type EWMAState =
    { rate               : float /// samples per nanosecond
      uncounted          : int64
      lastTickTime       : Instant option
      alphaPeriod        : Duration
      iclock             : IClock}

  /// Create a new EWMA state that you can do `update` and `tick` on.
  ///
  /// Alpha is dependent on the duration between sampling events ("how long
  /// time is it between the data points") so they are given as a pair.
  let create alphaPeriod iclock =
    { rate      = 0.
      uncounted = 0L
      lastTickTime = None
      alphaPeriod  = alphaPeriod
      iclock = iclock}

  let update state value =
    { state with uncounted = state.uncounted + value }

  let tick state =
    let instantNow = state.iclock.GetCurrentInstant()
    match state.lastTickTime with
    | None -> { state with lastTickTime = Some instantNow }
    | Some lastTickTime ->
      let samplePeriod = instantNow - lastTickTime
      let count = float state.uncounted
      let lastRate = state.rate
      let instantRate = if samplePeriod = Duration.Zero then 0. else count / samplePeriod.TotalNanoseconds
      let alpha = alpha samplePeriod state.alphaPeriod
      let rate = if lastRate = 0. then instantRate else lastRate + alpha * (instantRate - lastRate)
      //printfn "count %s samplePeriod %A instantRate %s lastRate %s alpha %s rate %s" 
      //  (string count) samplePeriod (string instantRate) (string lastRate) (string alpha) (string rate)
      { state with uncounted = 0L
                   rate      = rate
                   lastTickTime = Some instantNow}

  let rateInUnit (inUnit : Duration) state =
    state.rate * inUnit.TotalNanoseconds

  /// duration: SamplePeriod
  let oneMinuteEWMA iclock =
    create (Duration.FromMinutes 1L) iclock

  /// duration: SamplePeriod
  let fiveMinutesEWMA iclock =
    create (Duration.FromMinutes 5L) iclock

  /// duration: SamplePeriod
  let fifteenMinuteEWMA iclock =
    create (Duration.FromMinutes 15L) iclock