namespace Logary.Internals

open Logary
open NodaTime

/// A type giving more information about the service that this logary instance
/// is running on.
type RuntimeInfo =
  { /// Name of the service. Will show up as 'service' in e.g. Logstash/Kibana and
    /// is the basis for a lot of the sorting and health checking that Riemann
    /// does.
    serviceName : string

    clock       : IClock

    /// An internal logger for logary to use
    logger      : Logger }

  static member create (serviceName, clock) =
    { serviceName = serviceName
      clock       = clock
      logger      = NullLogger() }