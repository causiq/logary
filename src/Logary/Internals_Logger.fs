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
    member x.logVerbose fLine = ()
    member x.logDebug fLine = ()
    member x.log line = ()
    member x.level = Fatal
    member x.name = PointName.ofList [ "Logary"; "Internals"; "NullLogger" ]

module internal Logging =
  open Logary.Target

  type LoggerInstance =
    { name    : PointName
      targets : (MessageFilter * TargetInstance) list
      level   : LogLevel
      ilogger : Logger }
    with
      interface Named with
        member x.name = x.name

      interface Logger with
        member x.level: LogLevel = x.level

        member x.logVerbose fMsg =
          if Verbose >= x.level then
            let logger : Logger = upcast x
            logger.log (fMsg ()) // delegate down

        member x.logDebug fMsg =
          if Debug >= x.level then
            let logger : Logger = upcast x
            logger.log (fMsg ()) // delegate down

        member x.log msg =
          for accept, t in x.targets do
            if accept msg then
              Job.Global.start (Ch.give t.reqCh (Log msg))

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

    member x.logDebug fLine =
      if Debug >= x.lvl then
        let logger : Logger = upcast x
        logger.log (fLine ())

    member x.log msg =
      try
        if msg.level >= x.lvl then
          x.trgs |> List.iter (Target.send msg >> ignore)
      with _ ->
        ()

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

  /// Safely try to execute f, catching any exception thrown and logging that
  /// exception internally. Returns unit, irregardless of the codomain of f.
  let safe label ilogger f =
    try let x = f () in ()
    with e -> Message.error label |> Message.addExn e |> Logger.log ilogger

  /// Safely try to execute f, catching a thrown the exception type specified
  /// by the generic type parameter and logging that exception internally.
  /// Other exception types are not caught. Returns unit, irregardless of the
  /// codomain of f.
  let safeT<'a when 'a :> exn> label ilogger f =
    try let x = f () in ()
    with :? 'a as e -> Message.error label |> Message.addExn e |> Logger.log ilogger

  /// Safely try to execute asynchronous function f, catching any thrown
  /// exception and logging exception internally. Returns async<unit>
  /// irregardless of the codomain of f.
  let safeAsync label ilogger (f: unit -> Job<_>) = job {
    try
      let! x = f ()
      return ()
    with e -> Message.error label |> Message.addExn e |> Logger.log ilogger }

  /// Safely try to execute asynchronous function f, catching any thrown exception and logging
  /// that exception internally.
  /// Returns Unit irregardless of the codomain of f, by evaluating the async synchronously
  /// on the calling synchronisation context.
  /// WARNING: do not call inside a synchronisation context that is used to run asynchronous
  /// workflows, or you will block all async going on.
  let safeAsyncForce label ilogger f =
    safeAsync label ilogger f |> Job.Global.run
