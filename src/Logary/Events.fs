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

  let inline start () =
    fun cont -> cont >> Some

  let inline map (f:'a -> 'b) pipe =
    fun cont -> f >> cont |> pipe

  let inline filter (predicate:'a -> bool) pipe =
    fun cont -> pipe id >> Option.bind (fun prev -> if predicate prev then Some (cont prev) else None)

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