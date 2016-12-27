/// Use this to get a logger for your class/type/path that you want to send
/// metrics for or send logs from.
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

/// Gets a logger by a given point name.
[<CompiledName "GetLoggerByPointName">]
let getLoggerByPointName name =
  if box name = null then nullArg "name"
  Globals.getStaticLogger name :> Logger

/// Gets a logger by a given name.
[<CompiledName "GetLoggerByName">]
let getLoggerByName name =
  getLoggerByPointName (PointName.parse name)

/// Gets the current logger from the context that this method was called
/// in.
[<CompiledName "GetCurrentLogger"; MethodImpl(MethodImplOptions.NoInlining)>]
let getCurrentLogger () =
  getLoggerByPointName (getCurrentLoggerName ())
