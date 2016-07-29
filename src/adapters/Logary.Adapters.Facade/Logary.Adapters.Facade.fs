namespace Logary.Adapters.Facade

open Logary
open Hopac

/// This is the main adapter which logs from an arbitrary logary facade
/// into logary. Provide the namespace you put the facade in and the assembly
/// which it should be loaded from, and this adapter will use (memoized) reflection
/// to properly bind to the facade.
type LogaryFacadeAdapter(ns : string, asm : string) =
  interface Logary.Logger with
    member x.logVerboseWithAck messageFactory =
      Alt.always (Promise.Now.withValue ())

    member x.logDebugWithAck messageFactory =
      Alt.always (Promise.Now.withValue ())

    member x.logWithAck message =
      Alt.always (Promise.Now.withValue ())

    member x.logSimple message =
      ()

    member x.level =
      Fatal

    member x.name =
      PointName.ofList [ "Logary"; "Adapters"; "Facade" ]

  static member create ns asm =
    LogaryFacadeAdapter(ns, asm)