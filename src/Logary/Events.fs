namespace Logary

open System
open System.Text
open System.IO
open Logary
open Logary.Internals
open Hopac
open Hopac.Infixes

type Cancellation = internal { cancelled: IVar<unit> }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Cancellation =
  let create () =
    { cancelled = IVar () }

  let isCancelled cancellation =
    IVar.read cancellation.cancelled

  let cancel cancellation = 
    IVar.tryFill cancellation.cancelled ()

module Ticker = 

  /// 提供一个 支持 shutdown 的 alt 以供使用
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

  type Counter = 
    inherit Ticker<int64,Value*Units,Message []>

    override this.Folder state item =
      match item with 
      | Int64 i, _ when state = Int64.MaxValue && i > 0L ->
        state
      | Int64 i, _ when state = Int64.MinValue && i < 0L ->
        state
      | Int64 i, _ ->
        state + i
      | _ ->
        state

    override this.HandleTick state = 
      0L, [| Message.gaugeWithUnit (PointName.ofSingle "Todo") (Int64 state) Units.Scalar |]


[<RequireQualifiedAccessAttribute>]
module internal Pipe =

  open Ticker

  let start = fun cont -> cont >> Some

  let inline mapCont f flow = 
    fun cont -> id |> flow >> Option.bind (fun prev -> f prev cont)

  let inline map (f:'a -> 'b) pipe =
    pipe |> mapCont (fun prev cont -> prev |> f |> cont |> Some)

  let inline filter (predicate:'a -> bool) pipe =
    pipe |> mapCont (fun prev cont -> if predicate prev then Some (cont prev) else None)

  /// can be implement by tiked , then can support timespan
  let inline buffer n pipe =
    let results = new ResizeArray<_> ()
    pipe |> mapCont (fun prev cont -> 
      results.Add prev
      if results.Count >= n then
        let res = (List.ofSeq results)
        results.Clear ()
        Some (cont res)
      else 
        None)

  /// when some item comes in, it goes to ticker.folder, generate state
  /// when somewhere outside tick through ticker , ticker.handleTick generate new state and pipe result for continuation
  /// this fun will make pipe *async* through an background loop job 
  let inline tick (ticker:Ticker<'state,_,_>) pipe =
    fun cont -> 
      let updateMb = Mailbox ()
      
      let rec loop state =
        Alt.choose [
          ticker.Ticked ^=> fun _ ->
            let state', res = ticker.HandleTick state
            cont res
            loop state'

          updateMb ^=> (ticker.Folder state >> loop)
        ]
      loop ticker.InitialState |> Hopac.start

      id |> pipe >> Option.bind (fun prev -> updateMb *<<+ prev |> Hopac.start |> Some)
          
  /// give another name ?
  let inline counter counter pipe =
    tick counter pipe

  /// use ArraySegment instead ?
  let inline slidingWindow size pipe =
    let window = Array.zeroCreate size
    let slidingLen = size - 1
    let mutable count = 0u
    pipe |> mapCont (fun prev cont -> 
      Array.blit window 1 window 0 slidingLen
      window.[slidingLen] <- prev
      cont window |> Some)
  

[<RequireQualifiedAccessAttribute>]
module Events = 

  type T =
    private {
      pipes : (Message -> Message option) list
      // inputCh : Ch<LogLevel * (LogLevel -> Message)>
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

  let sink (targetName:string) pipe = 
    pipe (fun msg -> Message.setContext "target" targetName msg)

  let toPipes stream =
    stream.pipes
