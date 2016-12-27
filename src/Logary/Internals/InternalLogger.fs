namespace Logary.Internals

open System
open Hopac
open Hopac.Infixes
open Logary
open Logary.Target

module internal Logging =

  let send (targets : TargetInstance list) msg : Alt<Promise<unit>> =
    let latch = Latch targets.Length
    let targets = targets |> List.map (fun target -> target, IVar ())
    let traverse =
      targets
      |> List.traverseAltA (fun (target, ack) ->
        Alt.prepareJob <| fun () ->
        Job.start (ack ^=>. Latch.decrement latch) >>-.
        RingBuffer.put target.requests (Log (msg, ack)))
      |> Alt.afterFun (fun _ -> ())

    traverse ^->. memo (Latch.await latch)

  let logWithAck message : _ list -> Alt<Promise<unit>> = function
    | []      ->
      instaPromise
    | targets ->
      message |> send targets

  let inline ensureName (logger : Logger) (msg : Message) =
    match msg.name with
    | PointName [||] ->
      Message.setName logger.name msg
    | _  ->
      msg

  type LoggerInstance =
    { name    : PointName
      targets : (MessageFilter * TargetInstance) list
      level   : LogLevel }

    interface Logger with
      member x.name = x.name

      member x.level : LogLevel =
        x.level

      member x.log logLevel messageFactory =
        if logLevel >= x.level then
          let me : Logger = upcast x
          me.logWithAck logLevel messageFactory // delegate down
          |> Alt.afterFun (fun _ -> ())
        else
          Alt.always()

      member x.logWithAck logLevel messageFactory : Alt<Promise<unit>> =
        if logLevel >= x.level then
          let message = messageFactory logLevel |> ensureName x
          x.targets
          |> List.choose (fun (accept, t) -> if accept message then Some t else None)
          |> logWithAck message
        else
          Promise.instaPromise

/// This logger is special: in the above case the Registry takes the responsibility
/// of shutting down all targets, but this is a stand-alone logger that is used
/// to log everything in Logary with, so it needs to capable of handling its
/// own disposal. It must not throw under any circumstances.
type InternalLogger =
  { lvl  : LogLevel
    trgs : Target.TargetInstance list }

  interface IAsyncDisposable with
    member x.DisposeAsync() =
      x.trgs
      |> Seq.map Target.shutdown
      |> Job.conIgnore

  interface Logger with
    member x.log logLevel messageFactory =
      if logLevel >= x.lvl then
        let me : Logger = upcast x
        me.logWithAck logLevel messageFactory // delegate down
        |> Alt.afterFun (fun _ -> ())
      else
        Logging.insta

    member x.logWithAck logLevel messageFactory =
      if logLevel >= x.lvl then
        let message = messageFactory logLevel
        Logging.logWithAck message x.trgs
      else
        Logging.instaPromise

    member x.level =
      x.lvl

    member x.name =
      PointName.ofList ["Logary" ]

  static member create level (targets : #seq<_>) =
    { lvl = level; trgs = List.ofSeq targets }
    :> Logger