namespace Logary.Internals

open Logary

/// An internal interface; all globals in Logary are hidden and are managed by
/// the Registry and Config API.
type internal LoggingConfig =
  /// Gets a logger by name.
  abstract getLogger : PointName -> Logger
  /// Gets a logger by name and applies the passed middleware to it. You can
  /// also use `Logger.apply` on existing loggers to create new ones.
  abstract getLoggerWithMiddleware : PointName -> Middleware -> Logger
  /// Gets the current timestamp.
  abstract getTimestamp : unit -> EpochNanoSeconds
  /// Gets the console semaphore. When the process is running with an attached
  /// tty, this function is useful for getting the semaphore to synchronise
  /// around. You must take this if you e.g. make a change to the colourisation
  /// of the console output.
  abstract getConsoleSemaphore : unit -> obj

/// This module keeps track of the LoggingConfig reference.
module internal Global =
  open NodaTime

  type T =
    { getLogger               : PointName -> Logger
      getLoggerWithMiddleware : PointName -> Middleware -> Logger
      getTimestamp            : unit -> EpochNanoSeconds
      getConsoleSemaphore     : unit -> obj }
    with
      static member create getLogger getLoggerWM getTs getCS =
        { getLogger = getLogger
          getLoggerWithMiddleware = getLoggerWM
          getTimestamp = getTs
          getConsoleSemaphore = getCS }

      interface LoggingConfig with
        member x.getLogger pn = x.getLogger pn
        member x.getLoggerWithMiddleware pn mid = x.getLoggerWithMiddleware pn mid
        member x.getTimestamp () = x.getTimestamp ()
        member x.getConsoleSemaphore () = x.getConsoleSemaphore ()

  /// Null object pattern; will only return loggers that don't log.
  let defaultConfig =
    let c = SystemClock.Instance
    let s = obj ()
    let nl = NullLogger() :> Logger
    { getLogger = fun pn -> nl
      getLoggerWithMiddleware = fun pn mid -> nl
      getTimestamp = fun () -> c.GetCurrentInstant().ToUnixTimeTicks() * Constants.NanosPerTick
      getConsoleSemaphore = fun () -> s }

  /// This is the "Global Variable" containing the last configured Logary
  /// instance. If you configure more than one logary instance this will be
  /// replaced.
  let internal config =
    ref (defaultConfig, (* logical clock *) 1u)

  /// The flyweight references the current configuration. If you want
  /// multiple per-process logging setups, then don't use the static methods,
  /// but instead pass a Logger instance around, setting the name field of the
  /// Message value you pass into the logger.
  type Flyweight(name : PointName) =
    // The object's private fields are initialised to the current config's
    // logger.
    let updating = obj()
    let mutable fwClock : uint32 = snd !config
    let mutable logger : Logger = (fst !config).getLogger name

    /// A function that tries to run the action with the current logger, and
    /// which reconfigures if the configuration is updated.
    let withLogger action =
      if snd !config <> fwClock then // if we are outdated
        lock updating <| fun _ ->
          let cfg, cfgClock = !config // reread the config's clock after taking lock
          if cfgClock <> fwClock then // recheck after taking lock to avoid races
            logger <- cfg.getLogger name // get the current logger
            fwClock <- cfgClock // update instance's clock

      // finally execute the action with the logger
      action logger

    let ensureName (m : Message) =
      if m.name.isEmpty then { m with name = name } else m

    interface Logger with // flyweight
      member x.name = name

      member x.log level msgFactory =
        withLogger (fun logger -> logger.log level (msgFactory >> ensureName))

      member x.logWithAck level msgFactory =
        withLogger (fun logger -> logger.logWithAck level (msgFactory >> ensureName))

  // end of Flyweight

  // TODO: consider renaming to 'create' and returning a RunningService?

  /// Call to initialise Logary with a new Logary instance.
  let initialise cfg =
    config := (cfg, snd !config + 1u)

  let getStaticLogger (name : PointName) =
    Flyweight(name)

  /// Gets the current timestamp.
  let getTimestamp () : EpochNanoSeconds =
    (fst !config).getTimestamp ()

  /// Returns the synchronisation object to use when printing to the console.
  let getConsoleSemaphore () =
    (fst !config).getConsoleSemaphore()

  /// Run the passed function under the console semaphore lock.
  let lockSem fn =
    lock (getConsoleSemaphore ()) fn