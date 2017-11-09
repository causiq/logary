namespace Logary

open System
open System.Text
open System.IO
open Logary
open Logary.Internals
open Hopac
open Hopac.Infixes
open NodaTime

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
    let sum = s.values |> Array.map (fun d -> Math.Pow(float d - mean, 2.)) |> Array.sum
    sqrt (sum / float (size - 1))

type Cancellation = internal { cancelled: IVar<unit> }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Cancellation =
  let create () =
    { cancelled = IVar () }

  let isCancelled cancellation =
    IVar.read cancellation.cancelled

  let cancel cancellation =
    IVar.tryFill cancellation.cancelled ()


[<AbstractClass>]
type Ticker<'state,'t,'r> (initialState:'state) =
  let tickCh = Ch<unit> ()

  abstract member Folder     : 'state -> 't -> 'state
  abstract member HandleTick : 'state -> 'state * 'r

  member this.InitialState = initialState
  member this.Ticked = tickCh :> Alt<_>
  member this.Tick () = tickCh *<- ()

  member this.TickEvery timespan =
    let cancellation = Cancellation.create ()
    let rec loop () =
      Alt.choose [
        timeOut timespan ^=> fun _ ->
          this.Tick () ^=> fun _ ->
          loop ()

        Cancellation.isCancelled cancellation
      ]

    loop ()
    |> Job.start
    >>-. cancellation

type BufferTicker<'t> () =
  inherit Ticker<ResizeArray<'t>,'t,ResizeArray<'t>>(ResizeArray())
    override this.Folder state item =
      state.Add item
      state

    override this.HandleTick state =
      ResizeArray(),state


/// The an exponentially weighted moving average that gets ticks every
/// period (a period is a duration between events), but can get
/// `update`s at any point between the ticks.
module ExpWeightedMovAvg =
  open NodaTime

  /// calculate the alpha coefficient from a number of minutes
  ///
  /// - `samplePeriod` is how long is between each tick
  /// - `alphaPeriod` is the duration the EWMA should be calculated over
  let alpha (samplePeriod : Duration) (alphaPeriod : Duration) =
    1. - exp (- (float samplePeriod.Ticks / float alphaPeriod.Ticks))


  type EWMAState =
    { /// in samples per tick
      rate               : float
      uncounted          : int64
      lastTickTimestamp  : EpochNanoSeconds option
      alphaPeriod        : Duration}

  /// Create a new EWMA state that you can do `update` and `tick` on.
  ///
  /// Alpha is dependent on the duration between sampling events ("how long
  /// time is it between the data points") so they are given as a pair.
  let create (alphaPeriod) =
    { rate      = 0.
      uncounted = 0L
      lastTickTimestamp = None
      alphaPeriod  = alphaPeriod }

  let update state value =
    { state with uncounted = state.uncounted + value }

  let tick state =
    let timestampNow = Global.getTimestamp ()
    match state.lastTickTimestamp with
    | None -> { state with lastTickTimestamp = Some timestampNow
                           uncounted = 0L }
    | Some lastTickTimestamp ->
      let count = float state.uncounted
      let interval = timestampNow - lastTickTimestamp
      let samplePeriod = Duration.FromTicks (interval / Constants.NanosPerTick)
      let instantRate = count / float interval
      let currentRate = state.rate
      let alpha = alpha samplePeriod state.alphaPeriod
      let rate = currentRate + alpha * (instantRate - currentRate)
      { state with uncounted = 0L
                   rate      = rate }

  let rateInUnit (inUnit : Duration) state =
    state.rate * (float inUnit.Ticks / float Constants.NanosPerTick)


type EWMATicker<'t> (rateUnit, alphaPeriod) =
  inherit Ticker<ExpWeightedMovAvg.EWMAState,int64,float>(ExpWeightedMovAvg.create alphaPeriod)
    override this.Folder ewma item =
      ExpWeightedMovAvg.update ewma item

    override this.HandleTick ewma =
      let ewma' = ExpWeightedMovAvg.tick ewma
      let rate = ewma' |> ExpWeightedMovAvg.rateInUnit rateUnit
      ewma', rate

type PipeResult<'a> =
  internal
  | HasResult of 'a
  | NoResult
with
  member x.HasValue () =
    match x with | HasResult _ -> true | _ -> false
  member x.TryGet () =
    match x with | HasResult x -> Some x | _ -> None

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccessAttribute>]
module PipeResult =
  let hasValue (x: PipeResult<_>) =
    match x with | HasResult _ -> true | _ -> false

  let tryGet (x: PipeResult<_>) =
    match x with | HasResult x -> Some x | _ -> None

  let orDefault d x =
    match x with | HasResult x -> x | _ -> d


[<RequireQualifiedAccessAttribute>]
module Pipe =

  /// 'contInput means continuation function input
  /// 'contRes means continuation function output
  /// 'sourceItem means pipe source element
  /// when we have a pipe, we can pass a continuation to it,
  /// and then we can pipe source item to its builded processing
  type Pipe<'contInput,'contRes,'sourceItem> =
    private {
      build : cont<'contInput,'contRes> -> source<'sourceItem,'contRes>
      tickTimerJobs : Job<Cancellation> list
    }
  and private cont<'a,'b> = 'a -> PipeResult<'b> // just for better type annotation
  and private source<'a,'b> = 'a -> PipeResult<'b> // just for better type annotation
  and Processing = Pipe<Message,Alt<Promise<unit>>,Message> // for logary target based processing

  // let start<'sourceItem,'sourceRes> () =
  let start<'t,'r> =
    { build = id<cont<'t,'r>>
      tickTimerJobs = List.empty
    }

  /// when run cont for generate source, start all timer jobs in pipe at same time
  let run cont pipe =
    Job.conCollect pipe.tickTimerJobs
    >>- fun ctss ->
    let onNext = cont |> pipe.build
    (onNext, ctss)

  /// add a job to current pipe
  let withTickJob tickJob pipe =
    { pipe with tickTimerJobs = tickJob :: pipe.tickTimerJobs }

  let chain f pipe =
    { build = f >> pipe.build
      tickTimerJobs = pipe.tickTimerJobs
    }

  let composeForProcessingType (pipes: Processing list) =
    let allTickTimerJobs = List.collect (fun pipe -> pipe.tickTimerJobs) pipes
    let latch = Latch pipes.Length

    let build =
      fun cont ->
        let allBuildedSource = pipes |> List.map (fun pipe -> pipe.build cont)
        fun sourceItem ->
          let composed =
            allBuildedSource
            |> List.traverseAltA (fun onNext -> onNext sourceItem |> PipeResult.orDefault (Promise.instaPromise))
          composed
          ^-> (Hopac.Job.conIgnore >> memo)
          |> PipeResult.HasResult

    { build = build
      tickTimerJobs = allTickTimerJobs
    }

  let map f pipe =
    pipe |> chain (fun cont -> f >> cont)

  let filter predicate pipe =
    pipe |> chain (fun cont -> fun prev -> if predicate prev then cont prev else NoResult)

  let choose chooser pipe =
    pipe
    |> chain (fun cont -> fun prev -> match chooser prev with | Some mapped -> cont mapped | _ ->  NoResult)

  /// when some item comes in, it goes to ticker.folder, generate state
  /// when somewhere outside tick through ticker , ticker.handleTick generate new state and pipe input for continuation
  /// this fun will make pipe *async* through an background loop job, like some fire and forget style.
  /// so when user at callsite put some source item in, it return NoResult immediately.
  /// TODO : handle exception
  let tick (ticker:Ticker<'state,_,_>) (pipe: Pipe<_,#Job<_>,_>) =
    pipe
    |> chain (fun cont ->
       let updateMb = Mailbox ()

       let rec loop state =
         Alt.choose [
           ticker.Ticked ^=> fun _ ->
             try
               let state', item = ticker.HandleTick state
               item |> cont
               |> function | HasResult x -> x >>=. loop state'
                           | _ -> upcast (loop state')
             with
             | e ->
               upcast (loop state)

           updateMb ^=> (ticker.Folder state >> loop)
         ]

       // think about how to handle exception when ticker fun (folder/handletick throw exception)
       loop ticker.InitialState |> Hopac.server
       //  Job.supervise internalLogger (Policy.restart) (loop ticker.InitialState)

       fun prev ->
         Mailbox.Now.send updateMb  prev
         NoResult)

  let buffer n pipe =
    pipe
    |> chain (fun cont ->
       let results = new ResizeArray<_> ()
       fun prev ->
         results.Add prev
         if results.Count >= n then
           let res = (List.ofSeq results)
           results.Clear ()
           cont res
         else
           NoResult)

  let bufferTime timespan pipe =
    let ticker = BufferTicker ()
    pipe
    |> withTickJob (ticker.TickEvery timespan)
    |> tick ticker

  /// maybe use ArraySegment instead
  let slidingWindow size pipe =
    pipe
    |> chain (fun cont ->
       let window = Array.zeroCreate size
       let slidingLen = size - 1
       fun prev ->
         Array.blit window 1 window 0 slidingLen
         window.[slidingLen] <- prev
         cont window)

  /// use msg timestamp
  // let inline slidingWindowTime timespan pipe =
  //   pipe
  //   |> chain (fun cont ->
  //        let window = ResizeArray ()
  //        let slidingLen = size - 1
  //        let mutable count = 0u
  //        fun prev ->
  //          window.Add prev

  //          Array.blit window 1 window 0 slidingLen
  //          window.[slidingLen] <- prev
  //          cont window)



[<RequireQualifiedAccessAttribute>]
module Events =
  type T =
    private {
      pipes : Pipe.Processing list
    }

  let stream = {
    pipes = List.empty;
  }

  let events<'r> = Pipe.start<Message,'r>
  let subscribers pipes stream =
    {pipes = List.concat [pipes; stream.pipes;]}

  let service svc pipe =
    pipe |> Pipe.filter (fun msg -> Message.tryGetContext KnownLiterals.ServiceContextName msg = Some svc)

  let tag tag pipe = pipe |> Pipe.filter (Message.hasTag tag)

  let counter timespan pipe =
    failwith "todo"
    // pipe
    // |> Pipe.bufferTime timespan
    // |> Pipe.map (Seq.sumBy (fun (msg : Message) ->
    //    match msg.value with
    //    | Gauge (Int64 i, _)
    //    | Derived (Int64 i, _) -> i
    //    | _ -> 1L))

  let percentile quantile pipe =
    failwith "todo"

    // pipe
    // |> Pipe.map (fun msgs ->
    //    msgs |> Seq.map (fun (msg : Message) ->
    //    match msg.value with
    //    | Gauge (Int64 i, _)
    //    | Derived (Int64 i, _) -> i
    //    | _ -> 1L)
    //    |> Array.ofSeq
    //    |> Snapshot.create
    //    |> Snapshot.quantile
    //    <| quantile)

  let miniLevel level pipe =
    pipe |> Pipe.filter (fun msg -> msg.level >= level)

  let sink (targetName:string) pipe =
    pipe |> Pipe.map (Message.setContext "target" targetName)

  let toProcessing stream =
    stream.pipes |> Pipe.composeForProcessingType
