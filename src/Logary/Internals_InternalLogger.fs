/// For information on what F# StringFormat<'T> takes as arguments see
/// http://msdn.microsoft.com/en-us/library/vstudio/ee370560.aspx
///
/// TODO: make modular and configurable when Logary is started
namespace Logary.Internals

open System
open System.Globalization
open System.Diagnostics

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

type InternalLogger =
  { lvl  : LogLevel
    trgs : Targets.TargetInstance list }
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
  /// Safely try to execute f, catching any exception thrown and logging that exception internally. 
  /// Returns unit, irregardless of the codomain of f.
  let safeTry label f =
    try let x = f () in ()
    with e -> err "%O" e

  /// Safely try to execute f, catching a thrown the exception type specified by the generic type 
  /// parameter and logging that exception internally. Other exception types are not caught.
  /// Returns unit, irregardless of the codomain of f.
  let safeTryT<'a when 'a :> exn> label f =
    try let x = f () in ()
    with :? 'a as e -> err "%O" e

  /// Safely try to execute asynchronous function f, catching any thrown exception and logging
  /// that exception internally.
  /// Returns async<unit> irregardless of the codomain of f.
  let safeTryAsync label f = async {
    try
      let! x = f ()
      return ()
    with e -> err "%O" e }

  /// Safely try to execute asynchronous function f, catching any thrown exception and logging
  /// that exception internally.
  /// Returns Unit irregardless of the codomain of f, by evaluating the async synchronously
  /// on the calling synchronisation context.
  /// WARNING: do not call inside a synchronisation context that is used to run asynchronous
  /// workflows, or you will block all async going on.
  let safeTryAsyncForce label f =
    safeTryAsync label f |> Async.RunSynchronously
