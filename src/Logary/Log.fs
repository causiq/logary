namespace Logary

/// API for writing logs and metrics.
/// For gauges that run continuously based on a timer, have a look at
/// the Registry module. To get a logger, have a look at the Logger module.
module Log =
  open Logary.Internals

  open NodaTime

  module internal Map =
    let put k v (m : Map<_,_>) =
      match m.TryFind k with
      | None -> m |> Map.add k v
      | Some _ -> m |> Map.remove k |> Map.add k v

  //////////////////////////
  // Value/setter methods //
  //////////////////////////

  // Sets the path of the log line to the passed value
  let setPath p line = { line with LogLine.path = p }

  /// Create a new log line at the specified level for the given message
  let logLine level msg = LogLine.Create(msg, level)

  /// Add a tag 't' to the log line 'line'.
  let setTag t line = { line with tags = t :: line.tags }

  /// Add a key-value pair to the data
  let setData k v line = { line with data = line.data |> Map.put k v }

  /// Add the key-value pairs to the data to be logged
  let setDatas datas line =
    datas |> List.fold (fun s (k, v) -> s |> setData k v) line

  /// Set the LogLine's main exception property
  let setExn e line = { line with ``exception`` = Some e }

  /// A suggestion for an exception tag to send with log lines that
  /// are for exceptions
  [<Literal>]
  let ExceptionTag = "exn"

  /// Write a log entry from a log line.
  [<CompiledName "Log">]
  let log (logger : Logger) line =
    (line : LogLine)
    |> fun l -> match l.path with "" -> { l with path = logger.Name } | _ -> l
    |> logger.Log

  /// Write a verbose log entry, for help constructing format string, see:
  /// http://msdn.microsoft.com/en-us/library/vstudio/ee370560.aspx
  [<CompiledName "Verbose">]
  let verbosef logger fmt =
    log logger << Printf.kprintf (logLine Verbose) fmt

  /// Write a verbose log entry.
  [<CompiledName "VerboseString">]
  let verbose logger =
    log logger << logLine Verbose

  let verboseStr = logLine Verbose

  let verboseStrTag tag = setTag tag << logLine Verbose

  /// Write a debug log entry, for help constructing format string, see:
  /// http://msdn.microsoft.com/en-us/library/vstudio/ee370560.aspx
  [<CompiledName "Debug">]
  let debugf logger fmt =
    log logger << Printf.kprintf (logLine Debug) fmt

  /// Write a debug log entry.
  [<CompiledName "DebugString">]
  let debug logger =
    log logger << logLine Debug

  let debugStr = logLine Debug

  let debugStrTag tag = setTag tag << logLine Debug

  /// Write a info log entry, for help constructing format string, see:
  /// http://msdn.microsoft.com/en-us/library/vstudio/ee370560.aspx
  [<CompiledName "Info">]
  let infof logger fmt =
    log logger << Printf.kprintf (logLine Info) fmt

  /// Write an info log entry.
  [<CompiledName "InfoString">]
  let info logger =
    log logger << logLine Info

  let infoStr = logLine Info

  let infoStrTag tag = setTag tag << logLine Info

  /// Write a warn log entry, for help constructing format string, see:
  /// http://msdn.microsoft.com/en-us/library/vstudio/ee370560.aspx
  [<CompiledName "Warn">]
  let warnf logger fmt =
    log logger << Printf.kprintf (logLine Warn) fmt

  /// Write a warn log entry.
  [<CompiledName "WarnString">]
  let warn logger =
    log logger << logLine Warn

  let warnStr = logLine Warn

  let warnStrTag tag = setTag tag << logLine Warn

  /// Write a error log entry, for help constructing format string, see:
  /// http://msdn.microsoft.com/en-us/library/vstudio/ee370560.aspx
  [<CompiledName "Error">]
  let errorf logger fmt =
    log logger << Printf.kprintf (logLine Error)

  /// Write an error log entry.
  [<CompiledName "ErrorString">]
  let error logger =
    log logger << logLine Error

  let errorStr = logLine Error

  let errorStrTag tag = setTag tag << logLine Error

  /// Write a fatal log entry, for help constructing format string, see:
  /// http://msdn.microsoft.com/en-us/library/vstudio/ee370560.aspx
  [<CompiledName "Fatal">]
  let fatalf logger fmt =
    log logger << Printf.kprintf (logLine Fatal) fmt

  /// Write a fatal log entry.
  [<CompiledName "FatalString">]
  let fatal logger =
    log logger << logLine Fatal

  let fatalStr = logLine Fatal

  let fatalStrTag tag = setTag tag << logLine Fatal

  /////////////
  // TRACING //
  /////////////

  // TODO: tracing with tags

  /////////////
  // METRICS //
  /////////////

  /// Write a metric/measure
  [<CompiledName "Metric">]
  let metric logger ms =
    (logger : Logger).Metric ms

  /// Increment the counter at the path 'path'
  let incr logger path =
    { value     = 1.
    ; path      = path
    ; timestamp = Date.utcNow()
    ; level     = LogLevel.Info
    ; mtype     = MetricType.Counter }
    |> metric logger

  let incrBy logger path amount =
    { value     = amount
    ; path      = path
    ; timestamp = Date.utcNow ()
    ; level     = LogLevel.Info
    ; mtype     = MetricType.Counter }
    |> metric logger

  let decr logger path =
    { value     = -1.
    ; path      = path
    ; timestamp = Date.utcNow ()
    ; level     = LogLevel.Info
    ; mtype     = MetricType.Counter }
    |> metric logger

  let decrBy logger path amount =
    { value     = -amount
    ; path      = path
    ; timestamp = Date.utcNow ()
    ; level     = LogLevel.Info
    ; mtype     = MetricType.Counter }
    |> metric logger

  open System.Diagnostics

  /// Capture a timer metric with a given metric-level and metric-path.
  [<CompiledName "TimeLevel">]
  let timelvl (logger : Logger) lvl path f =
    if lvl < logger.Level then f ()
    else
      let now = Date.utcNow ()
      let sw = Stopwatch.StartNew()
      try
        f ()
      finally
        sw.Stop()
        { value     = sw.ElapsedTicks |> float
        ; path      = path
        ; timestamp = now
        ; level     = lvl
        ; mtype     = Timer(Duration.FromTicks(sw.ElapsedTicks)) }
        |> metric logger

  /// Capture a timer metric with a given metric-path
  [<CompiledName "Time">]
  let time logger path = timelvl logger LogLevel.Info path

  /// Capture a timer metric with the logger's name as the metric-path
  [<CompiledName "TimeLog">]
  let timelog logger = timelvl logger LogLevel.Info (logger.Name)

  /// Time a function execution with a 'path' equal to the passed argument.
  /// Path may be null, and is then replaced with the logger name
  [<CompiledName "TimePath">]
  let timePath (logger : Logger) lvl path (f : System.Func<_>) =
    let path = match path with null -> logger.Name | p -> p
    timelvl logger lvl path (fun () -> f.Invoke())
