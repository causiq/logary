namespace Logary.Internals

open Logary

/// A type giving more information about the service that this logary instance
/// is running on.
type RuntimeInfo =
  { /// Name of the service. Will show up as 'service' in e.g. LogStash/Kibana and
    /// is the basis for a lot of the sorting and health checking that Riemann
    /// does.
    serviceName : string

    /// An internal logger for logary to use
    logger      : Logger }