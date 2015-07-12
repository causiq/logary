module Logary.HealthChecks.WinPerfCounter

open Logary
open Logary.WinPerfCounter
/// Create a new HealthCheck from a WindowsPerfCounter record and a transformation
/// function `measureTransform`.
///
/// String.empty means no value as e.g. `instance`. Also takes a
/// `measureTransform` function that allows the caller to customize the value
/// in a Measure before returning. Suggested is to use `HealthChecks.setDesc`
/// to give the measure a nice description with detailed data.
let toHealthCheckNamed name wpc measureTransform =
  match toPC wpc with
  | Some counter ->
    { new HealthCheck with
        member x.Name = DP.joined name
        member x.GetValue () =
          try
            counter.NextValue()
            |> float
            |> measureTransform
            |> Measure.setPath name
            |> Measure.toResult
          with
            e -> NoValue
        member x.Dispose () =
          counter.Dispose() }
  | None -> mkDead name.joined

let toHealthCheck wpc =
  let name =
    [ wpc.category; wpc.counter ]
    @ match wpc.instance with | NotApplicable -> [] | Instance i -> [i]
  toHealthCheckNamed (DP name) wpc

/// Takes a list of IDisposable things (performance counters, perhaps?) and
/// wraps the call to Dispose() of the inner health check with calls to
/// Dispose for each of the resources
let hasResources (disposables : #IDisposable seq) (hc : HealthCheck) =
  { new HealthCheck with
      member x.Name = hc.Name
      member x.GetValue() = hc.GetValue()
      member x.Dispose() =
        disposables |> Seq.iter (fun d -> d.Dispose())
        hc.Dispose() }


