namespace Logary.Internals

open System.Net
open Logary
open NodaTime

/// A type giving more information about the service that this logary instance
/// is running on.
type RuntimeInfo =
  { /// Name of the service. Will show up as 'service' in e.g. Logstash/Kibana and
    /// is the basis for a lot of the sorting and health checking that Riemann
    /// does.
    serviceName : string
    /// The host name of the machine that is running Logary. This is almost
    /// always required to coordinate logs in a distributed system and is
    /// also useful when reading logs from multiple machines at the same time.
    host        : string
    /// The clock used internally inside Logary to get the current time.
    clock       : IClock
    /// An internal logger for logary to use
    logger      : Logger }

  /// Create a new RuntimeInfo record from the passed parameters.
  ///
  /// This function gives you the ability to pass a custom clock to use within
  /// logary, as well as a host name that the logary source has.
  static member create (serviceName, clock, ?host) =
    { serviceName = serviceName
      host        = defaultArg host (Dns.GetHostName())
      clock       = clock
      logger      = NullLogger() }