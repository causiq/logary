/// For information on what F# StringFormat<'T> takes as arguments see
/// http://msdn.microsoft.com/en-us/library/vstudio/ee370560.aspx
namespace Logary.Internals

open Logary

/// A logger that does absolutely nothing, useful for feeding into the target
/// that is actually *the* internal logger target, to avoid recursive calls to
/// itself.
type NullLogger() =
  interface logger with
    member x.Log line = ()
    member x.Measure measur = ()
    member x.Level = Fatal
    member x.Name = "Logary.Internals.NullLogger"

module internal Logging =
  open FSharp.Actor

  open Logary.Target

  /// A logger instance that keeps a reference to the actor targets that it
  /// logs to, as well as its name.
  type LoggerInstance =
    { name    : string
      targets : (LineFilter * MeasureFilter * IActor) list
      level   : LogLevel
      /// the internal logger for this logger instance
      ilogger : logger }
    with
      interface Named with
        member x.Name = x.name

      interface logger with
        member x.Log line =
          for accept, _, t in x.targets do
            try
              if accept line then
                t <-- Log line
            with
            | Actor.ActorInvalidStatus msg ->
              LogLine.errorf
                "Logging to %s failed, because of target state. Actor said: '%s'"
                x.name msg
              |> Logger.log x.ilogger

        member x.Measure m =
          for _, accept, t in x.targets do
            try
              if accept m then
                t <-- Measure m
            with
            | Actor.ActorInvalidStatus msg ->
              LogLine.errorf
                "Sending metric to %s failed, because of target state. Actor said: '%s'" 
                x.name msg
              |> Logger.log x.ilogger

        member x.Level =
          x.level

/// This logger is special: in the above case the Registry takes the responsibility
/// of shutting down all targets, but this is a stand-alone logger that is used
/// to log everything in Logary with, so it needs to capable of handling its
/// own disposal. It must not throw under any circumstances.
type InternalLogger =
  { lvl  : LogLevel
    trgs : Target.TargetInstance list }
  interface logger with
    member x.Log line =
      try
        if line.level >= x.lvl then
          x.trgs |> List.iter (flip Target.sendLogLine line)
      with _ -> ()
    member x.Measure (m : ``measure``) =
      try
        if m.m_level >= x.lvl then
          x.trgs |> List.iter (flip Target.sendMeasure m)
      with _ -> ()
    member x.Level =
      x.lvl
    member x.Name =
      "Logary.Internals.InternalLogger"
  interface System.IDisposable with
    member x.Dispose() =
      try
        x.trgs
        |> List.iter (Target.shutdown >> Async.Ignore >> Async.RunSynchronously)
      with _ -> ()
  static member Create(level, targets) =
    { lvl = level; trgs = targets } :> logger

module Try =
  /// Safely try to execute f, catching any exception thrown and logging that
  /// exception internally. Returns unit, irregardless of the codomain of f.
  let safe label ilogger f =
    try let x = f () in ()
    with e -> LogLine.error label |> LogLine.setExn e |> Logger.log ilogger

  /// Safely try to execute f, catching a thrown the exception type specified
  /// by the generic type parameter and logging that exception internally.
  /// Other exception types are not caught. Returns unit, irregardless of the
  /// codomain of f.
  let safeT<'a when 'a :> exn> label ilogger f =
    try let x = f () in ()
    with :? 'a as e -> LogLine.error label |> LogLine.setExn e |> Logger.log ilogger

  /// Safely try to execute asynchronous function f, catching any thrown
  /// exception and logging exception internally. Returns async<unit>
  /// irregardless of the codomain of f.
  let safeAsync label ilogger f = async {
    try
      let! x = f ()
      return ()
    with e -> LogLine.error label |> LogLine.setExn e |> Logger.log ilogger }

  /// Safely try to execute asynchronous function f, catching any thrown exception and logging
  /// that exception internally.
  /// Returns Unit irregardless of the codomain of f, by evaluating the async synchronously
  /// on the calling synchronisation context.
  /// WARNING: do not call inside a synchronisation context that is used to run asynchronous
  /// workflows, or you will block all async going on.
  let safeAsyncForce label ilogger f =
    safeAsync label ilogger f |> Async.RunSynchronously
