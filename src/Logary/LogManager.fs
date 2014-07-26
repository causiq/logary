namespace Logary

open NodaTime

open FSharp.Actor

open Logary.Internals

/// A type that encapsulates the moving parts of a configured Logary.
type LogaryInstance =
  { supervisor : IActor
    /// to use with Logary.Registry
    registry   : IActor
    /// to use with Logary.SCheduling (as the actor param)
    scheduler  : IActor
    metadata   : RuntimeInfo }

/// A type that gives information on how the shutdown went
type ShutdownState =
  { flushed  : Acks
    stopped  : Acks
    timedOut : bool }
  member x.Successful =
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
  inherit System.IDisposable

  /// Gets the service name that is used to filter and process the logs further
  /// downstream. This property is configured at initialisation of Logary.
  abstract RuntimeInfo : RuntimeInfo

  /// Get a logger denoted by the name passed as the parameter. This name can either be
  /// a specific name that you keep for a sub-component of your application or 
  /// the name of the class. Also have a look at Logging.GetCurrentLogger().
  abstract GetLogger : string -> Logger

  /// Awaits that all targets finish responding to a flush message
  /// so that we can be certain they have processed all previous messages.
  /// This function is useful together with unit tests for the targets.
  abstract FlushPending : Duration -> Acks

  /// Shuts Logary down after flushing, given a timeout duration to wait before
  /// counting the target as timed out in responding. The duration is applied
  /// to each actor's communication. Does an ordered shutdown.
  ///
  /// First duration: flush duration
  /// Second duration: shutdown duration
  /// Returns the shutdown book keeping info
  abstract Shutdown : Duration -> Duration -> ShutdownState