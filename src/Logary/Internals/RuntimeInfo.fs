namespace Logary.Internals

open Logary

/// A type giving more information about the service that this logary instance
/// is running on.
type RuntimeInfo =
  /// Name of the service. Will show up as 'service' in e.g. Logstash/Kibana and
  /// is the basis for a lot of the sorting and health checking that Riemann
  /// does.
  abstract service : string
  /// The host name of the machine that is running Logary. This is almost
  /// always required to coordinate logs in a distributed system and is
  /// also useful when reading logs from multiple machines at the same time.
  abstract host : string
  /// Gets the current timestamp
  abstract getTimestamp : unit -> EpochNanoSeconds
  /// Gets the console semaphore
  abstract getConsoleSemaphore : unit -> obj
  /// An internal logger for Logary's runtime, its {targets,metrics,...} to use.
  abstract logger : Logger