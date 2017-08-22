namespace Logary

open System
open System.Text
open System.IO
open Logary
open Logary.Internals
open Hopac
open Hopac.Infixes



[<RequireQualifiedAccessAttribute>]
module Pipe =

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

  /// when some item comes in, it goes to folder, generate state
  /// when somewhere outside send tick to tickCh , handleTick generate new state and pipe result for continuation
  /// some timeout pipe may implement by this 
  /// this fun will make pipe *async* through an background iter job 
  let inline ticked (folder: 'state -> 't -> 'state ) 
           initial 
           (handleTick:'state -> 'state * 'cr)
           (tickCh : Ch<_>)
           pipe =

    fun cont -> 
      let updateMb = Mailbox ()
      
      Job.iterateServer initial <| fun (state : 'state) ->
          Alt.choose [
            tickCh ^-> fun _ ->
              let state', res = handleTick state
              cont res
              state'

            updateMb ^-> (folder state)
          ]
      |> Hopac.start

      id |> pipe >> Option.bind (fun prev -> updateMb *<<+ prev |> Hopac.start |> Some)
          
  /// implement counter  ???
  let inline counter pn tickCh pipe =
    let reducer state = function
      | Int64 i, _ when state = Int64.MaxValue && i > 0L ->
        state
      | Int64 i, _ when state = Int64.MinValue && i < 0L ->
        state
      | Int64 i, _ ->
        state + i
      | _ ->
        state

    let ticker state =
      0L, [| Message.gaugeWithUnit pn (Int64 state) Units.Scalar |]

    ticked reducer 0L ticker tickCh pipe

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

  // let toProcessing (stream:T) = 
  //   (fun (msg:Message) (subsribers:HashMap<string, Message -> Job<unit>>) -> 
  //     (stream.subscriptions
  //     |> List.traverseJobA (fun source -> 
  //         let processedMsg = source msg
  //         match processedMsg with 
  //         | None -> Job.result ()
  //         | Some msg ->
  //           let targetName = Message.tryGetContext "target" msg
  //           match targetName with 
  //           | Some (String targetName) ->
  //             let subscriber = HashMap.tryFind targetName subsribers 
  //             match subscriber with
  //             | Some subscriber -> subscriber msg
  //             | _ -> Job.result ()
  //           | _ -> Job.result ()
  //       )
  //     )
  //     >>- ignore
  //     )
  //   |> Processing