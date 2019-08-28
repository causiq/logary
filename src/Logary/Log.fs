/// Use this to get a logger for your class/type/path that you want to send
/// metrics for or send logs from.
namespace Logary

open System
open Logary
open Logary.Internals

type Log =
  /// Creates a logger by a given point name.
  [<CompiledName "Create">]
  static member create name: Logger =
    if isNull (box name) then nullArg "name"
    Global.getStaticLogger name

  /// Creates a logger by a given name.
  [<CompiledName "Create">]
  static member create name: Logger =
    PointName.parse name
    |> Log.create

  /// Creates a logger for a given type.
  [<CompiledName "Create">]
  static member create (typ: Type): Logger =
    typ.FullName.Replace("+", ".")
    |> Log.create

  /// Craetes a logger by for a given type.
  [<CompiledName "Create">]
  static member create<'forType> () =
    Log.create typeof<'forType>