namespace Logary.Internals

open Hopac
open Logary
open Logary.Model


/// A type giving more information about the service that this logary instance
/// is running on.
type RuntimeInfo =
  /// Name of the service, location in the datacenter, etc...
  abstract resource: Resource
  /// Gets the current timestamp
  abstract getTimestamp: unit -> EpochNanoSeconds
  /// Gets the console semaphore
  abstract consoleLock: DVar<Lock>
  /// An internal logger for Logary's runtime, its {targets,metrics,...} to use.
  abstract logger: Logger

  /// Replaces the logger, returning a new instance with the new logger. Does not mutate this instance.
  abstract withLogger: logger: Logger -> RuntimeInfo

/// A data-structure that gives information about the outcome of a flush
/// operation on the Registry. This data structure is only relevant if the
/// flush operation had an associated timeout.
type FlushInfo = FlushInfo of acks:string list * timeouts:string list

/// A data-structure that gives information about the outcome of a shutdown
/// operation on the Registry. This data structure is only relevant if the
/// shutdown operation had an associated timeout.
type ShutdownInfo = ShutdownInfo of acks:string list * timeouts:string list
