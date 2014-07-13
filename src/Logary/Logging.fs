namespace Logary

/// Use this to get a logger for your class/type/path
/// that you want to send metrics for or send log lines from. For information
/// on what F# StringFormat<'T> takes as arguments see http://msdn.microsoft.com/en-us/library/vstudio/ee370560.aspx
module Logging =
  open System
  open System.Reflection
  open System.Diagnostics
  open System.Runtime.CompilerServices

  open FSharp.Actor

  open NodaTime

  open Logary.Registry.Logging
  open Logary.Internals
  open Logary.Internals.InternalLogger

  module Seq =
    let last xs = Seq.reduce (fun _ x -> x) xs

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
            logger :> logger
          | Some lm ->
            debug "getting logger by name: %s" name
            name |> Registry.getLogger lm.registry |> Async.RunSynchronously

  /// Gets the current logger from the context that this method was called
  /// in.
  [<CompiledName "GetCurrentLogger"; MethodImpl(MethodImplOptions.NoInlining)>]
  let getCurrentLogger () =
    getLoggerByName <| getCurrentLoggerName ()
