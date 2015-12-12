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

  let send targets msg =
    Alt.withNackJob <| fun nack ->
    let ack = IVar ()
    targets |> List.traverseJobA (fun target ->
      upcast (Log (msg, ack) |> Ch.give target.reqCh 
              <|> nack))
    >>-. ack

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

  interface System.IDisposable with
    member x.Dispose() =
      try
        x.trgs
        |> List.iter (Target.shutdown >> Job.Ignore >> Job.Global.run)
      with _ -> ()

  static member create level targets =
    { lvl = level
      trgs = targets }
    :> Logger

module Try =
  open Hopac.Infixes

  /// Safely try to execute asynchronous function f, catching any thrown
  /// exception and logging exception internally. Returns async<unit>
  /// irregardless of the codomain of f.
  let safe label logger (runnable: Job<_>) =
    Job.startIgnore (Job.catch runnable >>= (function
    | Choice1Of2 () ->
      Job.result ()

    | Choice2Of2 e ->
      Message.error label |> Message.addExn e |> Logger.log logger :> Job<_>))