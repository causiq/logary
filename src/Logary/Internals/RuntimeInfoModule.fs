namespace Logary.Internals

open Logary

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module RuntimeInfo =

  /// An implementation of the interface RuntimeInfo
  type T =
    internal { 
      host : string
      service : string
      getTimestamp : unit -> EpochNanoSeconds
      getConsoleSemaphore : unit -> obj
      logger : Logger
    }
    interface RuntimeInfo with
      member x.service = x.service
      member x.host = x.host
      member x.getTimestamp () = x.getTimestamp ()
      member x.getConsoleSemaphore () = x.getConsoleSemaphore ()
      member x.logger = x.logger

    static member create (other : RuntimeInfo) =
      { host = other.host
        service = other.service
        getTimestamp = other.getTimestamp
        getConsoleSemaphore = other.getConsoleSemaphore
        logger = other.logger }

  /// Create a new RuntimeInfo record from the passed parameters.
  ///
  /// This function gives you the ability to pass a custom clock to use within
  /// logary, as well as a host name that the logary source has.
  let create (service : string) (host : string) =
    { service = service
      host = host
      getTimestamp = Global.getTimestamp
      getConsoleSemaphore = Global.getConsoleSemaphore
      logger = NullLogger.instance }
    :> RuntimeInfo

  let setServiceName sn : RuntimeInfo -> RuntimeInfo = function
    | :? T as t ->
      { t with service = sn } :> _
    | other ->
      { T.create other with service = sn } :> _

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

  let setLogger logger : RuntimeInfo -> RuntimeInfo =
    function
    | :? T as t ->
      { t with logger = logger } :> _
    | other ->
      { T.create other with logger = logger } :> _