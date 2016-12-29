namespace Logary.Internals

open Logary

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module RuntimeInfo =

  /// An implementation of the interface RuntimeInfo
  type T =
    internal { 
      host : string
      serviceName : string
      getTimestamp : unit -> EpochNanoSeconds
      getConsoleSemaphore : unit -> obj
      logger : Logger
    }
    interface RuntimeInfo with
      member x.serviceName = x.serviceName
      member x.host = x.host
      member x.getTimestamp () = x.getTimestamp ()
      member x.getConsoleSemaphore () = x.getConsoleSemaphore ()
      member x.logger = x.logger

    static member create (other : RuntimeInfo) =
      { host = other.host
        serviceName = other.serviceName
        getTimestamp = other.getTimestamp
        getConsoleSemaphore = other.getConsoleSemaphore
        logger = other.logger }

  /// Create a new RuntimeInfo record from the passed parameters.
  ///
  /// This function gives you the ability to pass a custom clock to use within
  /// logary, as well as a host name that the logary source has.
  let create (serviceName : string) (host : string) (clock : NodaTime.IClock) =
    { serviceName = serviceName
      host = host
      getTimestamp = Globals.timestamp
      getConsoleSemaphore = Globals.semaphore
      logger = NullLogger.instance }
    :> RuntimeInfo

  let setServiceName sn : RuntimeInfo -> RuntimeInfo = function
    | :? T as t ->
      { t with serviceName = sn } :> _
    | other ->
      { T.create other with serviceName = sn } :> _

  let setHost host : RuntimeInfo -> RuntimeInfo = function
    | :? T as t ->
      { t with host = host } :> _
    | other ->
      { T.create other with host = host } :> _

  let setGetTimestamp fn : RuntimeInfo -> RuntimeInfo = function
    | :? T as t ->
      { t with getTimestamp = fn } :> _
    | other ->
      { T.create other with getTimestamp = fn } :> _

  let setGetConsoleSemaphore fn : RuntimeInfo -> RuntimeInfo = function
    | :? T as t ->
      { t with getConsoleSemaphore = fn } :> _
    | other ->
      { T.create other with getConsoleSemaphore = fn } :> _

  let setLogger logger : RuntimeInfo -> RuntimeInfo = function
    | :? T as t ->
      { t with logger = logger } :> _
    | other ->
      { T.create other with logger = logger } :> _
