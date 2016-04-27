namespace Logary

open System
open NodaTime
open Hopac
open Logary.Internals
open Logary.Supervisor
open Logary.Middleware

/// The messages that can be sent to the registry to interact with it and its
/// running targets.
type RegistryMessage =
  /// Get a logger for the given point name (the path of the logger)
  | GetLogger of logger:PointName * middleware:(unit -> Message -> Message) * replCh:IVar<Logger>

  /// Flush all pending messages from the registry to await shutdown and ack on
  /// the `ackCh` when done. If the client nacks the request, the `nack` promise
  /// is filled with a unit value.
  | FlushPending of ackCh:Ch<unit> * nack:Promise<unit>

  /// shutdown the registry in full
  | ShutdownLogary of ackCh:Ch<unit> * nack:Promise<unit>

type RegistryInstance =
  { reqCh : Ch<RegistryMessage> }

/// A type that encapsulates the moving parts of a configured Logary.
type LogaryInstance =
  { supervisor  : Supervisor.Instance
    /// to use with Logary.Registry
    registry    : RegistryInstance
    /// to use with Logary.SCheduling (as the actor param)
    scheduler   : Ch<Scheduling.ScheduleMsg>
    /// Runtime data, internal to Logary. E.g. the internal logger instance.
    runtimeInfo : RuntimeInfo
    /// Extra middleware
    middleware  : Mid list }

/// A type that gives information on how the shutdown went
type ShutdownState =
  { flushed  : Acks
    stopped  : Acks
    timedOut : bool }

  member x.successful =
    let succ = function Ack -> true | Nack _ -> false
    succ x.flushed && succ x.stopped

/// LogManager is the public interface to Logary and takes care of getting
/// loggers from names. It is also responsible for running Dispose at the
/// end of the application in order to run the target shutdown logic. That said,
/// the body of the software should be crash only, so even if you don't call dispose
/// terminating the application, it should continue working.
///
/// This is also a synchronous wrapper around the asynchronous actors that make
/// up logary
type LogManager =
  inherit IAsyncDisposable

  /// Gets the service name that is used to filter and process the logs further
  /// downstream. This property is configured at initialisation of Logary.
  abstract runtimeInfo : RuntimeInfo

  /// Get a logger denoted by the name passed as the parameter. This name can either be
  /// a specific name that you keep for a sub-component of your application or
  /// the name of the class. Also have a look at Logging.GetCurrentLogger().
  abstract getLogger : PointName -> Logger

  /// Awaits that all targets finish responding to a flush message
  /// so that we can be certain they have processed all previous messages.
  /// This function is useful together with unit tests for the targets.
  abstract flushPending : Duration -> Alt<unit>

  /// Shuts Logary down after flushing, given a timeout duration to wait before
  /// counting the target as timed out in responding. The duration is applied
  /// to each actor's communication. Does an ordered shutdown.
  ///
  /// First duration: flush duration
  /// Second duration: shutdown duration
  /// Returns the shutdown book keeping info
  abstract shutdown : Duration -> Duration -> Job<ShutdownState>