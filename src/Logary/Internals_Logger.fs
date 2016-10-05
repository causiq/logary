/// For information on what F# StringFormat<'T> takes as arguments see
/// http://msdn.microsoft.com/en-us/library/vstudio/ee370560.aspx
namespace Logary.Internals

open System
open Hopac
open Hopac.Infixes
open Logary
open Logary.Target

module internal Logging =

  let send (targets : _ list) msg : Alt<Promise<unit>> =
    let latch = Latch targets.Length

    let traverse =
      targets |> List.traverseAltA (fun target ->
        let ack = IVar ()
        Log (msg, ack) |> RingBuffer.put target.requests |> Alt.afterJob (fun () ->
          ack ^=>. Latch.decrement latch
        ))

    traverse ^->. memo (Latch.await latch)

  let instaPromise =
    Alt.always (Promise (())) // new promise with unit value

  let logWithAck message : _ list -> Alt<Promise<unit>> = function
    | []      ->
      instaPromise
    | targets ->
      message |> send targets

  type LoggerInstance =
    { name    : PointName
      targets : (MessageFilter * TargetInstance) list
      level   : LogLevel
      /// Internal logger
      ilogger : Logger }

    interface Named with
      member x.name = x.name

    interface Logger with
      member x.level: LogLevel =
        x.level

      member x.logVerboseWithAck msgFactory =
        if Verbose >= x.level then
          let logger : Logger = upcast x
          logger.logWithAck (msgFactory Verbose) // delegate down
        else
          instaPromise

      member x.logDebugWithAck msgFactory =
        if Debug >= x.level then
          let logger : Logger = upcast x
          logger.logWithAck (msgFactory Debug) // delegate down
        else
          instaPromise

      member x.logWithAck message : Alt<Promise<unit>> =
        x.targets
        |> List.choose (fun (accept, t) -> if accept message then Some t else None)
        |> logWithAck message

      member x.logSimple message : unit =
        x.targets
        |> List.choose (fun (accept, t) -> if accept message then Some t else None)
        |> logWithAck message
        |> Job.Ignore
        |> start

/// This logger is special: in the above case the Registry takes the responsibility
/// of shutting down all targets, but this is a stand-alone logger that is used
/// to log everything in Logary with, so it needs to capable of handling its
/// own disposal. It must not throw under any circumstances.
type InternalLogger =
  { lvl  : LogLevel
    trgs : Target.TargetInstance list }

  interface IAsyncDisposable with
    member x.DisposeAsync() =
      x.trgs |> Seq.map Target.shutdown |> Job.conIgnore

  interface Logger with
    member x.logVerboseWithAck msgFactory =
      if Verbose >= x.lvl then
        let logger : Logger = upcast x
        logger.logWithAck (msgFactory Verbose) // delegate down
      else
        Logging.instaPromise

    member x.logDebugWithAck msgFactory =
      if Debug >= x.lvl then
        let logger : Logger = upcast x
        logger.logWithAck (msgFactory Debug) // delegate down
      else
        Logging.instaPromise

    member x.logWithAck message =
      if message.level >= x.lvl then
        x.trgs |> Logging.logWithAck message
      else
        Logging.instaPromise

    member x.logSimple message =
      if message.level >= x.lvl then
        x.trgs |> Logging.logWithAck message |> Job.Ignore |> start

    member x.level =
      x.lvl

    member x.name =
      PointName.ofList ["Logary"; "Internals"; "InternalLogger" ]

  static member create level (targets : #seq<_>) =
    { lvl = level; trgs = List.ofSeq targets }
    :> Logger