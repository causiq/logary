namespace Logary.Internals

open Hopac
open Logary
open Logary.Internals

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module RuntimeInfo =

  /// An implementation of the interface RuntimeInfo
  type T =
    internal {
      host: string
      service: string
      getTimestamp: unit -> EpochNanoSeconds
      consoleLock: DVar<Lock>
      logger: Logger
    }
    interface RuntimeInfo with
      member x.service = x.service
      member x.host = x.host
      member x.getTimestamp () = x.getTimestamp ()
      member x.consoleLock = x.consoleLock
      member x.logger = x.logger

    static member create (other: RuntimeInfo) =
      { host = other.host
        service = other.service
        getTimestamp = other.getTimestamp
        consoleLock = other.consoleLock
        logger = other.logger }

  /// Create a new RuntimeInfo record from the passed parameters.
  ///
  /// This function gives you the ability to pass a custom clock to use within
  /// logary, as well as a host name that the logary source has.
  let create (service: string) (host: string): T =
    { service = service
      host = host
      getTimestamp = Global.getTimestamp
      consoleLock = Global.semaphoreD
      logger = NullLogger.instance }

  let setServiceName sn: RuntimeInfo -> RuntimeInfo = function
    | :? T as t ->
      { t with service = sn } :> _
    | other ->
      { T.create other with service = sn } :> _

  let setHost host: RuntimeInfo -> RuntimeInfo = function
    | :? T as t ->
      { t with host = host } :> _
    | other ->
      { T.create other with host = host } :> _

  let setGetTimestamp fn: RuntimeInfo -> RuntimeInfo = function
    | :? T as t ->
      { t with getTimestamp = fn } :> _
    | other ->
      { T.create other with getTimestamp = fn } :> _

  let setLogger logger: RuntimeInfo -> RuntimeInfo =
    function
    | :? T as t ->
      { t with logger = logger } :> _
    | other ->
      { T.create other with logger = logger } :> _