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
    (targets
     |> List.traverseAltA (fun target ->
      let ack = IVar ()
      (Log (msg, ack) |> RingBuffer.put target.requests)
      ^=>. Latch.decrement latch
    ))
    ^->. memo (Latch.await latch)

  let instaPromise =
    Alt.always (Promise.Now.withValue ())

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

      member x.logVerboseWithAck fMsg =
        if Verbose >= x.level then
          let logger : Logger = upcast x
          logger.logWithAck (fMsg ()) // delegate down
        else
          instaPromise

      member x.logDebugWithAck fMsg =
        if Debug >= x.level then
          let logger : Logger = upcast x
          logger.logWithAck (fMsg ()) // delegate down
        else
          instaPromise

      member x.logWithAck message : Alt<Promise<unit>> =
        x.targets
        |> List.choose (fun (accept, t) -> if accept message then Some t else None)
        |> logWithAck message

/// A logger that does absolutely nothing, useful for feeding into the target
/// that is actually *the* internal logger target, to avoid recursive calls to
/// itself.
type NullLogger() =
  interface Logger with
    member x.logVerboseWithAck fLine = Logging.instaPromise
    member x.logDebugWithAck fLine = Logging.instaPromise
    member x.logWithAck line = Logging.instaPromise
    member x.level = Fatal
    member x.name = PointName.ofList [ "Logary"; "Internals"; "NullLogger" ]

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
    member x.logVerboseWithAck fMsg =
      if Verbose >= x.lvl then
        let logger : Logger = upcast x
        logger.logWithAck (fMsg ()) // delegate down
      else
        Logging.instaPromise

    member x.logDebugWithAck fMsg =
      if Debug >= x.lvl then
        let logger : Logger = upcast x
        logger.logWithAck (fMsg ()) // delegate down
      else
        Logging.instaPromise

    member x.logWithAck message =
      x.trgs |> Logging.logWithAck message

    member x.level =
      x.lvl

    member x.name =
      PointName.ofList ["Logary"; "Internals"; "InternalLogger" ]

  static member create level (targets : #seq<_>) =
    { lvl = level; trgs = List.ofSeq targets }
    :> Logger