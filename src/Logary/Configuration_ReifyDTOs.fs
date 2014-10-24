namespace Logary.Configuration

open Logary

open Logary.Configuration
open Logary.Configuration.DTOs

open Logary.Target

[<AutoOpen>]
module ReifyDTOs =

  module internal Impl =
    let createTargets (targets : TargetDto list) (lc : LogaryConf) =
      targets
      |> List.map (fun t ->
                     let fac = 
                     Target.confTarget t.name)

    let createMetrics (metrics : MetricDto list) (lc : LogaryConf) =
      []

    

  open Impl

  let reify (conf : ConfDto) =
    confLogary conf.serviceName
    |> withTargets (createTargets conf.targets)
    |> withMetrics conf.pollPeriod (createMetrics conf.metrics)