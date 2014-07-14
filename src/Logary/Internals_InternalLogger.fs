/// For information on what F# StringFormat<'T> takes as arguments see
/// http://msdn.microsoft.com/en-us/library/vstudio/ee370560.aspx
///
/// TODO: make modular and configurable when Logary is started
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
    
//  let put lvl (m : string) : unit =
//    let n = DateTime.UtcNow.ToString("o", CultureInfo.InvariantCulture)
//    let msg = sprintf "[%s] (logary) %s: %s" lvl n m
//    (!Globals.write) msg
//    if Debugger.IsAttached then
//      Debugger.Log(LogLevel.Info.ToInt(), "Logary-Internal", m + Environment.NewLine)
module internal Logging =
  open FSharp.Actor

  open Logary.Targets

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
          // TODO: make functional:
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
          try
            (x.targets |> List.map (fun (_, _, x) -> x)) <-* Measure m
          with
          | Actor.ActorInvalidStatus msg ->
            LogLine.errorf
              "Sending metric to %s failed, because of target state. Actor said: '%s'" 
              x.name msg
            |> Logger.log x.ilogger

        member x.Level =
          x.level

type InternalLogger =
  { lvl  : LogLevel
    trgs : Targets.TargetInstance list }

  // TODO: replace with LoggerInstance from module internal Logging

  interface logger with
    member x.Log line =
      if line.level >= x.lvl then
        x.trgs |> List.iter (fun target -> line |> Targets.logTarget target)
    member x.Measure m =
      () // TODO: implement internal logging for measures
    member x.Level =
      x.lvl
    member x.Name =
      "Logary.Internals.InternalLogger"
  static member Create(level, targets) =
    { lvl = level; trgs = targets } :> logger

///// write a debug internal log entry
//let debug fmt = Printf.ksprintf (put "D") fmt
///// write an info internal log entry
//let info  fmt = Printf.ksprintf (put "I") fmt
///// write a warn internal log entry
//let warn  fmt = Printf.ksprintf (put "W") fmt
///// write an err internal log entry
//let err   fmt = Printf.ksprintf (put "E") fmt
///// write a fatal internal log entry
//let fatal fmt = Printf.ksprintf (put "F") fmt

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
