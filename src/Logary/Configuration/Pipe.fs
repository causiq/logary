namespace Logary.Configuration

open System
open Hopac
open Hopac.Infixes
open Logary.Configuration.Transformers

[<Struct>]
type PipeResult<'a> =
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

  let create x = HasResult x

  let map f x =
    match x with | HasResult x -> HasResult (f x) | _ -> NoResult

  let iter f x =
    match x with | HasResult x -> f x | _ -> ()

/// 'contInput means continuation function input
/// 'contRes means continuation function output
/// 'sourceItem means pipe source element
/// when we have a pipe, we can pass a continuation to it,
/// and then we can pipe source item to its builded processing
[<Struct>]
type Pipe<'contInput, 'contRes, 'sourceItem> =
  internal {
    build: cont<'contInput, 'contRes> -> source<'sourceItem, 'contRes>
    tickTimerJobs: Job<Cancellation> list
  }

and private cont<'a,'b> = 'a -> PipeResult<'b> // just for better type annotation
and private source<'a,'b> = 'a -> PipeResult<'b> // just for better type annotation

[<RequireQualifiedAccessAttribute>]
module Pipe =

  let start<'t,'r> =
    { build = id<cont<'t,'r>>
      tickTimerJobs = List.empty
    }

  let chain f pipe =
    { build = f >> pipe.build
      tickTimerJobs = pipe.tickTimerJobs
    }

  let map f pipe =
    pipe |> chain (fun cont -> f >> cont)

  let after f pipe =
    { build = pipe.build >> f
      tickTimerJobs = pipe.tickTimerJobs
    }


  /// As we start the Pipe running, we also spawn all the tickers, receiving their
  /// cancellation tokens in return.
  let run cont pipe =
    Job.conCollect pipe.tickTimerJobs >>- fun cancelTickers ->
    let k = pipe.build cont
    k, cancelTickers

  /// add a job to current pipe
  let withTickJob tickJob pipe =
    { pipe with tickTimerJobs = tickJob :: pipe.tickTimerJobs }


  let filter predicate pipe =
    pipe |> chain (fun cont -> fun prev -> if predicate prev then cont prev else NoResult)

  let choose chooser pipe =
    pipe
    |> chain (fun cont -> fun prev -> match chooser prev with | Some mapped -> cont mapped | _ ->  NoResult)

  /// when some item comes in, it goes to ticker.folder, generate state
  /// when somewhere outside tick through ticker , ticker.handleTick generate new state and pipe input for continuation
  /// this fun will make pipe *async* through an background loop job, like some fire and forget style.
  /// so when user at callsite put some source item in, it return NoResult immediately.
  /// TODO: handle exception
  let tick (ticker:Ticker<_,_,_>) pipe =
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
               // todo: handle exception
               eprintfn "%A" e
               upcast (loop state)

           updateMb ^=> (ticker.Folder state >> loop)
         ]

       // think about how to handle exception when ticker fun (folder/handletick throw exception)
       loop ticker.InitialState |> Hopac.server
       //  Job.supervise internalLogger (Policy.restart) (loop ticker.InitialState)

       fun prev ->
         // TODO: think about wait for ack, means blocking. may cause deadlock
         // http://hopac.github.io/Hopac/Hopac.html#def:val%20Hopac.Hopac.run
         // A call of run xJ is safe when the call is not made from within a Hopac worker thread
         // and the job xJ does not perform operations that might block or that might directly,
         // or indirectly, need to communicate with the thread from which run is being called.
         Mailbox.Now.send updateMb prev
         NoResult)

  let tickTimer (ticker: Ticker<_,_,_>) (timespan: TimeSpan) pipe =
    pipe
    |> withTickJob (ticker.TickEvery timespan)
    |> tick ticker

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
    pipe |> tickTimer ticker timespan

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

  let counter (mapping: _ -> int64) timespan pipe =
    pipe
    |> map mapping
    |> bufferTime timespan
    |> map (Seq.sum)

  let percentile (mapping: _ -> int64 array) quantile pipe =
    pipe
    |> map (mapping >> Snapshot.create >> (fun snapshot -> Snapshot.quantile snapshot quantile))


  type BufferAction = | Reset | Delivery | AddToBuffer

  let bufferConditional (deliveryDecider: _ * list<_> -> BufferAction) pipe =
    pipe
    |> chain (fun cont ->
       let buffer = new ResizeArray<_> ()

       fun prev ->
         let buffered = (List.ofSeq buffer)
         match deliveryDecider (prev, buffered) with
         | Reset ->
           buffer.Clear ()
           NoResult
         | Delivery ->
           buffer.Clear ()
           (buffered @ [prev]) |> cont
         | AddToBuffer ->
           buffer.Add(prev)
           NoResult
       )