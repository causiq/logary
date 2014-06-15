namespace Logary.Metric

/// Represents an integer that can be incremented and decremented
module Counter =
  open FSharp.Actor

  open Logary
  open Logary.Internals
  open Metrics
  open Targets

  /// Construct a new metric counter at the instant this function is called
  /// with the passed value and name
  let counterValue name amount =
    { value     = amount
      path      = name
      timestamp = Date.utcNow ()
      level     = LogLevel.Info
      mtype     = Counter
      data      = Map.empty }

  /// Logary's representation of a counter.
  type CounterInstance(name, targets) =
    inherit MetricInstance(name, targets)
    with
      interface Counter with
        member x.Inc amount = x.Targets <-* Metric( counterValue name (float amount))
        member x.Dec amount = x.Targets <-* Metric( counterValue name (float amount))

  /// Used by the registry to convert the target instance list to a
  /// counter instance.
  let internal fromTargets name (targets : (_ * TargetInstance * _) list) =
    CounterInstance(name, targets |> List.map (fun (_, ti, _) -> actor ti))

  let inc (c : CounterInstance) = c.Targets <-* Metric(counterValue (c.Name) 1.)
  let dec (c : CounterInstance) = c.Targets <-* Metric(counterValue (c.Name) -1.)
