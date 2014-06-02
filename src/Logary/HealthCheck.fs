namespace Logary

open Metrics

/// You can centralise the service's health checks by registering instances
/// of this interface.
type HealthCheck =
  inherit Named
  /// Performs a check with the health check.
  abstract Check : unit -> HealthCheckResult

/// A result of the health check. Either Healthy or Unhealthy
and HealthCheckResult =
  /// All nice and dandy.
  | Healthy
  /// Didn't go well.
  | Unhealthy of UnhealthyResult

/// The details an unhealthy result
and UnhealthyResult =
  /// Gets the message detailing what went badly with the evaluation of the health check.
  abstract Message : string
  /// Gets the optional exception that was thrown as a part of the evaluation of the
  /// health check.
  abstract Exception : exn option

