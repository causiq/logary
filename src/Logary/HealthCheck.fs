namespace Logary

open System

open Metrics

/// The details a result
type ResultData =
  /// If the health check is for a value (which it probably is) then it should
  /// contain the measure generated.
  abstract Measure : Measure

  /// Gets the description detailing what went badly with the evaluation of the
  /// health check. Useful for drilling down.
  abstract Description : string

  /// Gets the optional exception that was thrown as a part of the evaluation
  /// of the health check.
  abstract Exception   : exn option

/// A result of the health check. Either Healthy or Unhealthy
type HealthCheckResult =
  /// This health check has no value available.
  | NoValue
  /// The health check has a value.
  | HasValue of ResultData

/// You can centralise the service's health checks by registering instances
/// of this interface.
type HealthCheck =
  inherit Named
  inherit IDisposable

  /// Performs a check with the health check.
  abstract LastValue : unit -> HealthCheckResult
