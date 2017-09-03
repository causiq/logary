namespace Logary

open System
open System.Text
open System.IO
open Logary
open Logary.Internals
open Hopac
open Hopac.Infixes
open NodaTime

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
  inherit Ticker<List<'t>,'t,List<'t>>(List.empty)
    override this.Folder state item = 
      [yield! state; yield item]

    override this.HandleTick state =
      List.empty, state

type Pipe<'c,'r,'s> = 
  internal {
    run : ('c -> Job<'r>) -> ('s -> Alt<unit>)
    tickTimerJobs : Job<Cancellation> list
  }


[<RequireQualifiedAccessAttribute>]
module Pipe =

  let start =
    let run = fun cont -> 
      let sourceCh = Ch ()

      let rec loop () =
        Alt.choose [
          sourceCh ^=> fun (source,reply) ->
            cont source
            >>=. reply *<= () 
            >>=. loop ()
        ]

      loop () |> Hopac.start

      fun source ->
        sourceCh *<+=>- (fun reply -> (source,reply)) 

    {
      run = run
      tickTimerJobs = List.empty
    }

  let run cont pipe =
    Job.conCollect pipe.tickTimerJobs 
    >>- fun ctss ->
    let onNext = pipe.run cont
    (onNext, ctss)

  let withTickJob tickJob pipe =
    { pipe with tickTimerJobs = tickJob :: pipe.tickTimerJobs }

  let chain f pipe =
    {
      run = f >> pipe.run
      tickTimerJobs = pipe.tickTimerJobs
    } 

  let inline map (f:'a -> 'b) pipe =
    pipe 
    |> chain (fun cont -> f >> cont) 

  let inline filter (predicate:'a -> bool) pipe =
    pipe 
    |> chain (fun cont -> fun prev -> if predicate prev then cont prev else Job.result ())


  /// when some item comes in, it goes to ticker.folder, generate state
  /// when somewhere outside tick through ticker , ticker.handleTick generate new state and pipe result for continuation
  /// this fun will make pipe *async* through an background loop job 
  let inline tick (ticker:Ticker<'state,_,_>) pipe =
    pipe
    |> chain (fun cont -> 
         let updateMb = Mailbox ()
         
         let rec loop state =
           Alt.choose [
             ticker.Ticked ^=> fun _ ->
               let state', res = ticker.HandleTick state
               cont res
               >>=. loop state'
   
             updateMb ^=> (ticker.Folder state >> loop)
           ]
           
         loop ticker.InitialState |> Hopac.start
   
         fun prev -> updateMb *<<+ prev)


  let inline buffer n pipe =
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
             Job.result ())


  let inline bufferTime timespan pipe =
    let ticker = BufferTicker ()
    pipe
    |> withTickJob (ticker.TickEvery timespan)
    |> tick ticker


  /// maybe use ArraySegment instead
  let inline slidingWindow size pipe =
    pipe
    |> chain (fun cont ->
         let window = Array.zeroCreate size
         let slidingLen = size - 1
         let mutable count = 0u
         fun prev -> 
           Array.blit window 1 window 0 slidingLen
           window.[slidingLen] <- prev
           cont window)
  

type Processing = Pipe<Message,unit,Message>

[<RequireQualifiedAccessAttribute>]
module Events = 

  type T =
    private {
      pipes : Processing list
    }

  let stream = {
    pipes = List.empty;
  }

  let subscribers pipes stream =
    {pipes = List.concat [pipes; stream.pipes;]}

  let service svc pipe = 
    pipe |> Pipe.filter (fun msg ->
      msg.context |> HashMap.tryFind KnownLiterals.ServiceContextName = Some (Logary.String svc))

  let tag tag pipe = pipe |> Pipe.filter (Message.hasTag tag)

  let counter timespan pipe =
    pipe 
    |> Pipe.bufferTime timespan 
    |> Pipe.map (fun msgs -> 
      let sumResult = msgs |> Seq.sumBy (fun (msg : Message) -> 
        match msg.value with
        | Gauge (Int64 i, _) 
        | Derived (Int64 i, _) -> i
        | _ -> 1L)
      Message.event Info (sprintf "counter result is %i within %s" sumResult (timespan.ToString ())))

  let miniLevel level pipe =
    pipe |> Pipe.filter (fun msg -> msg.level >= level)

  let sink (targetName:string) pipe = 
    pipe |> Pipe.map (Message.setContext "target" targetName)

  let toProcessing stream =
    let pipes = stream.pipes
    let allTickTimerJobs = List.collect (fun pipe -> pipe.tickTimerJobs) pipes

    let run = fun cont -> 
      let allOnNextFuns = List.map (fun pipe -> pipe.run cont) pipes
      fun prev -> 
        Hopac.Extensions.Seq.Con.iterJobIgnore (fun onNext -> onNext prev) allOnNextFuns
        |> memo 
        :> Alt<_>

    {
      run = run
      tickTimerJobs = allTickTimerJobs
    }