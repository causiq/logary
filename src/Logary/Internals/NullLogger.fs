namespace Logary.Internals

open Hopac
open Logary

/// A logger that does absolutely nothing, useful for feeding into the target
/// that is actually *the* internal logger target, to avoid recursive calls to
/// itself.
type private NullLogger() =
  interface Logger with // null logger
    member x.logWithAck logLevel messageFactory = Promise.altAlwaysTrue
    member x.level = LogLevel.Fatal
    member x.name = PointName.ofList [ "Logary"; "NullLogger" ]

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module NullLogger =
  let instance = NullLogger () :> Logger