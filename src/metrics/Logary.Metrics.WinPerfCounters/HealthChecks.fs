module Logary.HealthChecks.WinPerfCounter

open System
open Logary
open Logary.HealthCheck
open Logary.Metrics
open Logary.Metrics.WinPerfCounter
open Hopac

/// Create a new HealthCheck from a WindowsPerfCounter record and a transformation
/// function `measureTransform`.
///
/// String.empty means no value as e.g. `instance`. Also takes a
/// `measureTransform` function that allows the caller to customize the value
/// in a Measure before returning. Suggested is to use `HealthChecks.setDesc`
/// to give the measure a nice description with detailed data.
let toHealthCheckNamed healthCheckName wpc measureTransform =
  match toWindowsCounter wpc with
  | Some wpci ->
    { new HealthCheck with
        member x.name =
          healthCheckName

        member x.getValue () =
          try
            wpci.nextValues()
            |> measureTransform wpc.baseName
            |> HealthCheckResult.ofMessage
            |> Job.result
          with
            e -> Job.result NoValue

        member x.DisposeAsync () =
          wpci.counter.Dispose()
          Job.result () }
  | None ->
    createDead healthCheckName

let toHealthCheck (wpc : WinPerfCounter) =
  toHealthCheckNamed wpc.baseName wpc

/// Takes a list of IDisposable things (performance counters, perhaps?) and
/// wraps the call to Dispose() of the inner health check with calls to
/// Dispose for each of the resources
let hasResources (disposables : #IDisposable seq) (hc : HealthCheck) =
  { new HealthCheck with
      member x.name = hc.name
      member x.getValue() = hc.getValue()
      member x.DisposeAsync() =
        disposables |> Seq.iter (fun d -> d.Dispose())
        hc.DisposeAsync() }