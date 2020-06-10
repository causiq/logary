namespace Logary.Internals

open Hopac
open Logary

/// A type giving more information about the service that this logary instance
/// is running on.
type RuntimeInfo =
  /// Name of the service. Will show up as 'service' in e.g. Logstash/Kibana and
  /// is the basis for a lot of the sorting and health checking that Riemann
  /// does.
  abstract service: string // TODO: convert to Resource
  /// The host name of the machine that is running Logary. This is almost
  /// always required to coordinate logs in a distributed system and is
  /// also useful when reading logs from multiple machines at the same time.
  abstract host: string
  /// Gets the current timestamp
  abstract getTimestamp: unit -> EpochNanoSeconds
  /// Gets the console semaphore
  abstract consoleLock: DVar<Lock>
  /// An internal logger for Logary's runtime, its {targets,metrics,...} to use.
  abstract logger: Logger


/// A data-structure that gives information about the outcome of a flush
/// operation on the Registry. This data structure is only relevant if the
/// flush operation had an associated timeout.
type FlushInfo = FlushInfo of acks:string list * timeouts:string list

/// A data-structure that gives information about the outcome of a shutdown
/// operation on the Registry. This data structure is only relevant if the
/// shutdown operation had an associated timeout.
type ShutdownInfo = ShutdownInfo of acks:string list * timeouts:string list
