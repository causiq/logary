/// For information on what F# StringFormat<'T> takes as arguments see
/// http://msdn.microsoft.com/en-us/library/vstudio/ee370560.aspx
namespace Logary.Internals

open Hopac
open Logary

/// A logger that does absolutely nothing, useful for feeding into the target
/// that is actually *the* internal logger target, to avoid recursive calls to
/// itself.
type NullLogger() =
  interface Logger with
    member x.logVerbose fLine = Alt.always ()
    member x.logDebug fLine = Alt.always ()
    member x.log line = Alt.always ()
    member x.level = Fatal
    member x.name = PointName.ofList [ "Logary"; "Internals"; "NullLogger" ]

module internal Logging =
  open Logary.Target
  open Hopac.Infixes

  let send targets msg : Alt<_> =
    printfn "sending msg"
    Alt.withNackJob <| fun nack ->
      let res : Alt<_> =
        (targets |> List.traverseAltA (fun target -> Alt.prepareJob <| fun _ ->
          let ack = IVar ()
          target.reqCh *<+ Log (msg, ack) >>- (fun _ -> printfn "nack full? %b" nack.Full) >>-.
          (ack ^=> (fun _ -> printfn "ack Log"; ack) <|>
           nack ^-> (fun _ -> printfn "nack Log")))
        )
        ^->. ()
      Job.result () >>- (fun () -> printfn "send job called"; res)

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

        member x.logDebug fMsg =
          if Debug >= x.level then
            let logger : Logger = upcast x
            logger.log (fMsg ()) // delegate down
          else
            Alt.always ()

        member x.log msg : Alt<unit> =
          let targets =
            x.targets
            |> List.choose (fun (accept, t) -> if accept msg then Some t else None)

          msg |> send targets

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

    member x.logDebug fLine =
      if Debug >= x.lvl then
        let logger : Logger = upcast x
        logger.log (fLine ())
      else
        Alt.always ()

    member x.log msg =
      try
        if msg.level >= x.lvl then
          msg |> Logging.send x.trgs
        else
          Alt.always ()
      with _ ->
        Alt.always ()

    member x.level =
      x.lvl

    member x.name =
      PointName.ofList ["Logary"; "Internals"; "InternalLogger" ]

  static member create level targets =
    { lvl = level
      trgs = targets }
    :> Logger
