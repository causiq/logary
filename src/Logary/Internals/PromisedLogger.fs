namespace Logary.Internals

open Hopac
open Logary

/// The promised logger is constructed through a the asynchronous call to
/// getLogger (i.e. the call to the Registry's getLogger channel). Every
/// call will return a job that is started on the global scheduler, which
/// assumes that the promise will be returned at some point in the (close)
/// future. If this assumption does not hold, we'll get an issue where all
/// of the log-method calls will put work on the global Hopac scheduler,
/// which in turn causes the 'unbounded queue' problem. However, it's
/// safe to assume that the promise will be completed shortly after the c'tor
/// of this type is called.
type internal PromisedLogger(name, requestedLogger : Job<Logger>) =
  let promised = memo requestedLogger

  /// Create a new `Logger` with the given name and `Job<Logger>` – nice to
  /// use for creating loggers that need to be immediately available.
  static member create (PointName contents as name) (logger : Job<Logger>) =
    if logger = null then nullArg "logger"
    if contents = null then nullArg "name"
    PromisedLogger(name, logger) :> Logger

  interface Logger with
    member x.name = name

    member x.logWithAck logLevel messageFactory =
      Promise.read promised
      |> Alt.afterJob (fun logger -> logger.logWithAck logLevel messageFactory)

    member x.log logLevel messageFactory =
      Promise.read promised
      |> Alt.afterJob (fun logger -> logger.logWithAck logLevel messageFactory)
      |> Alt.afterFun (fun _ -> ())

    member x.level =
      Verbose