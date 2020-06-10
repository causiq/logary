namespace Logary

open Logary.Internals
open Logary.Metric
open NodaTime
open Hopac

/// LogManager is the public interface to Logary and takes care of getting
/// loggers from names. It is also responsible for running Dispose at the
/// end of the application in order to run the target shutdown logic. That said,
/// the body of the software should be crash only, so even if you don't call dispose
/// terminating the application, it should continue working.
///
/// This is also a synchronous wrapper around the asynchronous actors that make
/// up logary
type LogManager =
  /// Gets the service name that is used to filter and process the logs further
  /// downstream. This property is configured at initialisation of Logary.
  abstract runtimeInfo: RuntimeInfo

  /// Get a logger denoted by the name passed as the parameter. This name can either be
  /// a specific name that you keep for a sub-component of your application or
  /// the name of the class. Also have a look at Logging.GetCurrentLogger().
  abstract getLogger: PointName -> Logger

  /// Awaits that all targets finish responding to a flush message
  /// so that we can be certain they have processed all previous messages.
  /// This function is useful together with unit tests for the targets.
  abstract flushPending: Duration -> Alt<FlushInfo>
  abstract flushPending: unit -> Alt<unit>

  /// Shuts Logary down after flushing, given a timeout duration to wait before
  /// counting the target as timed out in responding. The duration is applied
  /// to each actor's communication. Does an ordered shutdown.
  ///
  /// First duration: flush duration
  /// Second duration: shutdown duration
  /// Returns the shutdown book keeping info
  abstract shutdown: flush:Duration * shutdown:Duration -> Alt<FlushInfo * ShutdownInfo>
  abstract shutdown: unit -> Alt<unit>

  /// Dynamically controls logger min level,
  /// this will only affect the loggers (its name, not its instance) that have been created before
  abstract switchLoggerLevel: string * LogLevel -> unit

  /// The manager-global Metrics registry
  abstract metricRegistry: MetricRegistry

[<AutoOpen>]
module LogManagerEx =

  type LogManager with
    member x.getLogger (loggerName: string) =
      x.getLogger (PointName.parse loggerName)
