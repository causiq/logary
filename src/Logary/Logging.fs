/// Use this to get a logger for your class/type/path
/// that you want to send metrics for or send log lines from. For information
/// on what F# StringFormat<'T> takes as arguments see http://msdn.microsoft.com/en-us/library/vstudio/ee370560.aspx
module Logary.Logging

// see https://github.com/Hopac/Hopac/commit/5a60797462deff83135fa0ee02403671045774ff#commitcomment-18537866
#nowarn "44"

open System
open System.Reflection
open System.Diagnostics
open System.Diagnostics.Contracts
open System.Runtime.CompilerServices
open Hopac
open Hopac.Infixes
open NodaTime
open Logary
open Logary.Registry.Logging
open Logary.Internals

/// Gets the current name, as defined by the class-name + namespace that the logger is in.
[<CompiledName "GetCurrentLoggerName"; MethodImpl(MethodImplOptions.NoInlining); Pure>]
let getCurrentLoggerName () =
  let toSkip = 2
  let getName (meth : MethodBase, dt : Type) =
    match dt with
    | null -> PointName.ofSingle meth.Name
    | _ -> PointName.parse dt.FullName
  let sf, m = let sf' = StackFrame(toSkip, false) in sf', sf'.GetMethod()
  let cont (dt : Type) = not <| (dt = null) && dt.Module.Name.Equals("mscorlib.dll", StringComparison.OrdinalIgnoreCase)
  let frame_still_unrolling_or_is_in_dotnet_core = cont << snd
  let successor s = Some (s+1, s+1)

  Seq.unfold successor toSkip
  |> Seq.map (fun framesToSkip -> let m = StackFrame(framesToSkip, false).GetMethod() in m, m.DeclaringType)
  |> Seq.takeWhile frame_still_unrolling_or_is_in_dotnet_core
  |> Seq.append (Seq.singleton (m, m.DeclaringType)) // unless we found something, put the default in there
  |> Seq.last
  |> getName

type PromisedLogger (name, requestedLogger : Job<Logger>) =
  let promised = memo requestedLogger

  interface Named with
    member x.name = name

  interface Logger with
    member x.logVerboseWithAck fMessage =
      Promise.read promised
      |> Alt.afterJob (fun logger -> logger.logVerboseWithAck fMessage)

    member x.logDebugWithAck fMessage =
      Promise.read promised
      |> Alt.afterJob (fun logger -> logger.logDebugWithAck fMessage)

    member x.logWithAck message =
      Promise.read promised
      |> Alt.afterJob (fun logger -> logger.logWithAck message)

    member x.logSimple message =
      Promise.read promised
      |> Alt.afterJob (fun logger -> logger.logWithAck message)
      |> Job.Ignore
      |> start

    member x.level =
      Verbose

[<CompiledName "GetLoggerByPointName">]
let getLoggerByPointName name =
  if box name = null then nullArg "name"
  match !Globals.singleton with
  | None ->
    let logger = Flyweight name :> FlyweightLogger
    Globals.addFlyweight logger
    logger :> Logger

  | Some inst ->
    Logger.logVerbose inst.runtimeInfo.logger (fun _ ->
      Message.eventVerbosef "Getting logger by name '%O'" name)
    >>=. (name |> Registry.getLogger inst.registry)
    // this should be the only location we actually do the run call, because
    // we absolutely need it initialising static variables synchronously at
    // the call-site
    |> fun l -> PromisedLogger(name, l) :> Logger

/// Gets a logger by a given name.
[<CompiledName "GetLoggerByName">]
let getLoggerByName name =
  getLoggerByPointName (PointName.parse name)

/// Gets the current logger from the context that this method was called
/// in.
[<CompiledName "GetCurrentLogger"; MethodImpl(MethodImplOptions.NoInlining)>]
let getCurrentLogger () =
  getLoggerByPointName (getCurrentLoggerName ())
