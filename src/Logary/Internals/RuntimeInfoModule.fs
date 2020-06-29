namespace Logary.Internals

open Hopac
open Logary
open Logary.Internals
open Logary.Model

/// An implementation of the interface RuntimeInfo
type RuntimeInfoValue =
  { resource: Resource
    getTimestamp: unit -> EpochNanoSeconds
    consoleLock: DVar<Lock>
    logger: Logger }

  interface RuntimeInfo with
    member x.resource = x.resource
    member x.getTimestamp () = x.getTimestamp ()
    member x.consoleLock = x.consoleLock
    member x.logger = x.logger
    member x.withLogger logger = { x with logger = logger } :> RuntimeInfo

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module RuntimeInfo =

  let create resource =
    { resource=resource
      getTimestamp=Global.getTimestamp
      consoleLock=Global.lockD
      logger=NullLogger.instance }

  let ofOther (other: RuntimeInfo) =
    { resource = other.resource
      getTimestamp = other.getTimestamp
      consoleLock = other.consoleLock
      logger = other.logger }

  let ofServiceAndHost service host =
    create { service=service; detail=Hostname host :: [] }

  let setGetTimestamp (getTimestamp: _) (ri: RuntimeInfoValue) =
    { ri with getTimestamp = getTimestamp }