namespace Logary.Internals

open Hopac
open Logary

/// A logger that does absolutely nothing, useful for feeding into the target
/// that is actually *the* internal logger target, to avoid recursive calls to
/// itself.
[<Sealed>]
type NullLogger private () =
  static let inst = lazy (NullLogger() :> Logger)
  static let success = Alt.always (Result.Ok Promise.unit)

  static member instance = inst.Value

  interface Logger with
    member x.logWithAck(_, _) = success
    member x.level = LogLevel.Fatal
    member x.name = PointName.ofList [ "Logary"; "NullLogger" ]
