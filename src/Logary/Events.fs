namespace Logary

open Logary
open Logary.Internals
open Hopac
open Hopac.Infixes


type Source<'s> =
  abstract onNext:  's -> Alt<unit>
  
type Cont<'t> = 't -> Alt<unit>

type Flow<'s,'t> =
  internal { run: Cont<'t> -> Source<'s> }


[<RequireQualifiedAccessAttribute>]
module Flow =

  let inline internal Flow f = { run = f }

  module Internals = 
    // Public permanent entrypoint to implement inlined versions of map, filter, choose etc.
    // 'f' is called with one argument before iteration.
    let mapCont f flow =
      Flow (fun cont ->
       { new Source<'T> with
          member x.onNext item =
            let source = flow.run ( f cont )
            source.onNext item
      })

  let inline start<'T> =
     Flow (fun cont -> 
      { new Source<'T> with
          member x.onNext item =
            cont item
      })

  // Used to indicate that we don't want a closure to be curried
  let inline internal nocurry() = Unchecked.defaultof<unit>

  let inline map (f:'T -> 'R) (flow:Flow<_,'T>) : Flow<_,'R> =
    flow |> Internals.mapCont (fun cont -> nocurry(); f >> cont)

  let inline filter (predicate:'T -> bool) (flow:Flow<_,'T>) : Flow<_,'T> =
    flow |> Internals.mapCont (fun cont -> nocurry(); fun prev -> if predicate prev then cont prev else Alt.always ())


[<RequireQualifiedAccessAttribute>]
module Events = 

  type T =
    private {
      subscriptions : Flow<Message,Message> list
      // inputCh : Ch<LogLevel * (LogLevel -> Message)>
    }

  let stream = {
    subscriptions = List.empty;
  }

  let flow = Flow.start<Message>

  let subscribers pipes stream =
    let pipes = List.concat [pipes; stream.subscriptions;]
    {subscriptions = pipes}

  let service svc flow = 
    flow |> Flow.filter (fun msg -> msg.context |> HashMap.tryFind KnownLiterals.ServiceContextName = Some (String svc))

  let tag tag flow = flow |> Flow.filter (Message.hasTag tag)

  let sink (targetName:string) flow = 
    flow |> Flow.map (fun msg -> Message.setContext "target" targetName msg)


  /// 把 stream 转化为对 msg 的处理，应该是遍历订阅的各种处理流程，进行处理
  let toProcessing (stream:T) = 
    (fun (msg:Message) (emitCh:Ch<Message>) -> 
      (stream.subscriptions
      |> List.traverseAltA (fun flow -> 
          // this will not compile, case we need Alt<unit> , 
          // should we change Source and Cont type in async style ? 
          // e.g.  type Cont<'t> = 't -> Alt<unit>  
           let source = flow.run (fun msg -> Ch.give emitCh msg)
           source.onNext msg
        )
      )
      ^-> ignore)
      |> Processing