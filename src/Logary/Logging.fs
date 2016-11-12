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
open Logary.Message
open Logary.Registry.Logging
open Logary.Internals

/// Gets the current name, as defined by the class-name + namespace that the logger is in.
[<CompiledName "GetCurrentLoggerName"; MethodImpl(MethodImplOptions.NoInlining); Pure>]
let getCurrentLoggerName () =
  let getName (meth : MethodBase, dt : Type) =
    if isNull meth then nullArg "meth"
    if isNull dt then nullArg "dt"
    match dt with
    | null -> PointName.ofSingle meth.Name
    | _    -> PointName.parse dt.FullName

  let toSkip = 2
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

/// The promised logger is constructed through a the asynchronous call to
/// getLogger (i.e. the call to the Registry's getLogger channel). Every
/// call will return a job that is started on the global scheduler, which
/// assumes that the promise will be returned at some point in the (close)
/// future. If this assumption does not hold, we'll get an issue where all
/// of the log-method calls will put work on the global Hopac scheduler,
/// which in turn causes the 'unbounded queue' problem. However, it's
/// safe to assume that the promise will be completed shortly after the c'tor
/// of this type is called.
type PromisedLogger(name, requestedLogger : Job<Logger>) =
  let promised = memo requestedLogger

  /// Create a new `Logger` with the given name and `Job<Logger>` – nice to
  /// use for creating loggers that need to be immediately available.
  static member create (PointName contents as name) logger =
    if logger = null then nullArg "logger"
    if contents = null then nullArg "name"
    PromisedLogger(name, logger) :> Logger

  // interface implementations;

  interface Named with
    member x.name = name

  interface Logger with
    member x.logWithAck logLevel messageFactory =
      Promise.read promised
      |> Alt.afterJob (fun logger -> logger.logWithAck logLevel messageFactory)

    member x.log logLevel messageFactory =
      Promise.read promised
      |> Alt.afterJob (fun logger -> logger.logWithAck logLevel messageFactory)
      |> Job.Ignore
      |> start

    member x.logSimple message =
      Promise.read promised
      |> Alt.afterJob (fun logger -> logger.logWithAck message.level (fun _ -> message))
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
    inst.runtimeInfo.logger.verbose (
      eventX "Getting logger by name {name}" >> setField "name" (name.ToString()))
    |> start

    name
    |> Registry.getLogger inst.registry
    |> PromisedLogger.create name

/// Gets a logger by a given name.
[<CompiledName "GetLoggerByName">]
let getLoggerByName name =
  getLoggerByPointName (PointName.parse name)

/// Gets the current logger from the context that this method was called
/// in.
[<CompiledName "GetCurrentLogger"; MethodImpl(MethodImplOptions.NoInlining)>]
let getCurrentLogger () =
  getLoggerByPointName (getCurrentLoggerName ())
