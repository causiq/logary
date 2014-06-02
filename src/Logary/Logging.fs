namespace Logary

/// Use this to get a logger for your class/type/path
/// that you want to send metrics for or send log lines from. For information
/// on what F# StringFormat<'T> takes as arguments see http://msdn.microsoft.com/en-us/library/vstudio/ee370560.aspx
module Logging =
  open FSharp.Actor
  open NodaTime
  open System

  open Targets
  open Logary.Internals
  open Logary.Internals.InternalLogger
  open Logary.Internals.Date

  /// A logger instance that keeps a reference to the actor tagets
  /// that it logs to, as well as its name.
  type LoggerInstance =
    { name    : string
    ; targets : (Acceptor * IActor) list
    ; level   : LogLevel }
    with
      interface Named with
        member x.Name = x.name

      interface Logger with
        member x.Log line =
          // TODO: make functional:
          for accept, t in x.targets do
            try
              if accept line then
                t <-- Log line
            with
            | Actor.ActorInvalidStatus msg ->
              err "Logging to %s failed, because of target state. Actor said: '%s'" x.name msg

        member x.Metric m =
          try
            (x.targets |> List.map snd) <-* Metric m
          with
          | Actor.ActorInvalidStatus msg ->
            err "Sending metric to %s failed, because of target state. Actor said: '%s'" x.name msg

        member x.Level =
          x.level

  /// Flyweight logger impl - reconfigures to work when it is configured, and until then
  /// throws away all log lines.
  type private FWL(name) =
    let locker = obj()
    let logManager = ref None
    let logger = ref None
    interface FlyweightLogger with
      member x.Configured lm =
        lock locker (fun () ->
          logManager := Some lm
          logger := Some <| lm.GetLogger(name))
    interface Logger with
      member x.Name = name
      member x.Log l = (!logger) |> Option.iter (fun logger -> logger.Log l)
      member x.Metric m = (!logger) |> Option.iter (fun logger -> logger.Metric m)
      member x.Level = Verbose

  /// Iterate through all flywieghts and set the current LogManager for them
  let private goFish () =
    lock Globals.criticalSection <| fun () ->
        match !Globals.singleton with
        | None -> ()
        | Some lm ->
          !Globals.flyweights |> List.iter (fun f -> f.Configured lm)
          Globals.flyweights := []

  /// Singleton configuration entry point: call from the runLogary code.
  let internal logaryRun logManager =
    lock Globals.criticalSection <| fun () ->
        debug "Logging.logaryRun w/ logManager instance"
        Globals.singleton := Some logManager
        goFish ()

  /// Singleton configuration exit point: call from shutdownLogary code
  let internal logaryShutdown _ =
    lock Globals.criticalSection <| fun () ->
        Globals.singleton := None
        Globals.flyweights := []

  open System.Reflection
  open System.Diagnostics
  open System.Runtime.CompilerServices

  module private Seq = let last = Seq.reduce (fun _ x -> x)
  let private successor s = Some (s+1, s+1)
  let private toSkip = 2
  let private (|IsNull|_|) value = if obj.ReferenceEquals(value, null) then Some() else None

  ////////////////
  // PUBLIC API //
  ////////////////

  /// Gets the current name, as defined by the class-name + namespace that the logger is in.
  [<CompiledName "GetCurrentLoggerName"; MethodImpl(MethodImplOptions.NoInlining); System.Diagnostics.Contracts.Pure>]
  let getCurrentLoggerName () =
    let getName (meth : MethodBase, dt : Type) = match dt with null -> meth.Name | _ -> dt.FullName
    let sf, m = let sf' = StackFrame(toSkip, false) in sf', sf'.GetMethod()
    let cont (dt : Type) = not <| (dt = null) && dt.Module.Name.Equals("mscorlib.dll", StringComparison.OrdinalIgnoreCase)
    let frame_still_unrolling_or_is_in_dotnet_core = cont << snd

    Seq.unfold successor toSkip
    |> Seq.map (fun framesToSkip -> let m = StackFrame(framesToSkip, false).GetMethod() in m, m.DeclaringType)
    |> Seq.takeWhile frame_still_unrolling_or_is_in_dotnet_core
    |> Seq.append (Seq.singleton (m, m.DeclaringType)) // unless we found something, put the default in there
    |> Seq.last
    |> getName

  /// Gets a logger by a given name.
  [<CompiledName "GetLoggerByName">]
  let getLoggerByName name =
    match name with
    | IsNull    -> nullArg "name"
    | _ as name ->
      lock Globals.criticalSection <| fun () ->
          match !Globals.singleton with
          | None ->
            debug "getting logger flyweight by name: %s" name
            let logger = FWL name :> FlyweightLogger
            Globals.flyweights := logger :: !Globals.flyweights
            logger :> Logger
          | Some lm ->
            debug "getting logger by name: %s" name
            lm.GetLogger name

  /// Gets the current logger from the context that this method was called
  /// in.
  [<CompiledName "GetCurrentLogger"; MethodImpl(MethodImplOptions.NoInlining)>]
  let getCurrentLogger () =
    getLoggerByName <| getCurrentLoggerName ()

  /// Create a new Logger from the targets passed, with a given name.
  [<CompiledName "FromTargets">]
  let fromTargets name (targets : (Acceptor * TargetInstance * LogLevel) list) =
    { name    = name
    ; targets = targets |> List.map (fun (a, ti, _) -> a, (Targets.actor ti))
    ; level   = targets |> List.map (fun (_, _, level) -> level) |> List.min }
    :> Logger
