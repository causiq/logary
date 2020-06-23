/// This module keeps track of the LoggingConfig reference.
module Logary.Internals.Global

open Hopac
open NodaTime
open Logary
open Logary.Metric

type T =
  { getLogger: PointName -> Logger
    getTimestamp: unit -> EpochNanoSeconds
    /// Gets the console semaphore. When the process is running with an attached tty, this function is useful for
    /// getting the semaphore to synchronise around. You must take this if you e.g. make a change to the colourisation
    /// of the console output.
    consoleLock: Lock
    metrics: MetricRegistry }

/// Null object pattern; will only return loggers that don't log.
let defaultConfig =
  { getLogger = fun _ -> NullLogger.instance
    getTimestamp = MonotonicClock.getTimestamp
    consoleLock = Lock()
    metrics = MetricRegistry() }

/// This is the "Global Variable" containing the last configured Logary
/// instance. If you configure more than one logary instance this will be
/// replaced.
let internal configD = DVar.create defaultConfig

let internal clockD =
  let createClock config = { new IClock with member x.GetCurrentInstant() = Instant.ofEpoch (config.getTimestamp()) }
  configD |> DVar.map createClock

let internal lockD =
  configD |> DVar.map (fun x -> x.consoleLock)

/// The flyweight references the current configuration. If you want multiple per-process logging setups, then don't use
/// the static methods, but instead pass a Logger instance around, setting the name field of the Message value you pass
/// into the Logger.
type Flyweight(name: PointName) =
  let loggerD = configD |> DVar.map (fun cfg -> cfg.getLogger name)
  interface Logger with // flyweight
    member x.name = name
    member x.level =
      let logger = DVar.get loggerD in logger.level
    member x.logWithAck(putBufferTimeOut, message) =
      let logger = DVar.get loggerD
      logger.logWithAck(putBufferTimeOut, message)

/// Call to initialise Logary with a new Logary instance.
let initialise cfg = DVar.set configD cfg

let getStaticLogger (name: PointName) = Flyweight(name) :> Logger

/// Gets the current timestamp.
let getTimestamp =
  configD |> DVar.mapFun (fun config -> config.getTimestamp)

let getMetricRegistry () =
  let config = DVar.get configD
  config.metrics