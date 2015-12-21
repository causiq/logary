/// For information on what F# StringFormat<'T> takes as arguments see
/// http://msdn.microsoft.com/en-us/library/vstudio/ee370560.aspx
namespace Logary.Internals

open Hopac
open Hopac.Infixes
open Logary
open Logary.Target

/// A logger that does absolutely nothing, useful for feeding into the target
/// that is actually *the* internal logger target, to avoid recursive calls to
/// itself.
type NullLogger() =
  interface Logger with
    member x.logVerbose fLine = Alt.always ()
    member x.logVerboseWithAck fLine = Alt.always (Promise.Now.withValue ())
    member x.logDebug fLine = Alt.always ()
    member x.logDebugWithAck fLine = Alt.always (Promise.Now.withValue ())
    member x.log line = Alt.always ()
    member x.logWithAck line = Alt.always (Promise.Now.withValue ())
    member x.level = Fatal
    member x.name = PointName.ofList [ "Logary"; "Internals"; "NullLogger" ]

module internal Logging =
  let send (targets : _ list) msg : Alt<Promise<unit>> =
    let latch = Latch targets.Length
    printfn "sending msg"
    (targets |> List.traverseAltA (fun target ->
      let ack = IVar ()
      ((Log (msg, ack)) |> BoundedMb.put target.requests)
      ^=>. Latch.decrement latch
    ))
    ^->. memo (Latch.await latch)

  type LoggerInstance =
    { name    : PointName
      targets : (MessageFilter * TargetInstance) list
      level   : LogLevel
      ilogger : Logger }
    with
      interface Named with
        member x.name = x.name

      interface Logger with
        member x.level: LogLevel =
          x.level

        member x.logVerbose fMsg =
          if Verbose >= x.level then
            let logger : Logger = upcast x
            logger.log (fMsg ()) // delegate down
          else
            Alt.always ()

        member x.logVerboseWithAck fMsg =
          failwith "not implemented"

        member x.logDebug fMsg =
          if Debug >= x.level then
            let logger : Logger = upcast x
            logger.log (fMsg ()) // delegate down
          else
            Alt.always ()

        member x.logDebugWithAck fMsg =
          failwith "not implemented"

        member x.log msg : Alt<unit> =
          (x :> Logger).logWithAck msg ^->. ()

        member x.logWithAck msg : Alt<Promise<unit>> =
          let targets =
            x.targets
            |> List.choose (fun (accept, t) ->
              if accept msg then Some t else None)

          match targets with
          | []      -> Alt.always (Promise.Now.withValue ())
          | targets -> msg |> send targets

/// This logger is special: in the above case the Registry takes the responsibility
/// of shutting down all targets, but this is a stand-alone logger that is used
/// to log everything in Logary with, so it needs to capable of handling its
/// own disposal. It must not throw under any circumstances.
type InternalLogger =
  { lvl  : LogLevel
    trgs : Target.TargetInstance list }

  interface Logger with
    member x.logVerbose fLine =
      if Verbose >= x.lvl then
        let logger : Logger = upcast x
        logger.log (fLine ())
      else
        Alt.always ()

    member x.logVerboseWithAck fLine =
      failwith "not implemented"

    member x.logDebug fLine =
      if Debug >= x.lvl then
        let logger : Logger = upcast x
        logger.log (fLine ())
      else
        Alt.always ()

    member x.logDebugWithAck fLine =
      failwith "not implemented"

    member x.log msg =
      if msg.level >= x.lvl then
        (msg |> Logging.send x.trgs) ^->. ()
      else
        Alt.always ()

    member x.logWithAck msg =
      failwith "not implemented"

    member x.level =
      x.lvl

    member x.name =
      PointName.ofList ["Logary"; "Internals"; "InternalLogger" ]

  static member create level targets =
    { lvl = level
      trgs = targets }
    :> Logger
