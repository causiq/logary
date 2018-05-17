namespace Logary

open NodaTime
open Hopac
open Hopac.Infixes

module Alt =

  let timeFun onComplete onNack (xA: Alt<'a>) =
    Alt.withNackJob (fun nack ->
      let ts = StopwatchTicks.getTimestamp ()
      let markNack =
        nack |> Alt.afterFun (fun () ->
        let now = StopwatchTicks.getTimestamp ()
        onNack (Gauge.ofStopwatchTicks (now - ts)))
      let altCommit =
        xA |> Alt.afterFun (fun x ->
        let now = StopwatchTicks.getTimestamp ()
        onComplete (Gauge.ofStopwatchTicks (now - ts))
        x)
      Job.start markNack >>-. altCommit)

  let timeJob onComplete onNack (xA: Alt<'a>) =
    Alt.withNackJob (fun nack ->
      let ts = StopwatchTicks.getTimestamp ()
      let markNack =
        nack |> Alt.afterJob (fun () ->
        let now = StopwatchTicks.getTimestamp ()
        onNack (Gauge.ofStopwatchTicks (now - ts)))
      let altCommit =
        xA |> Alt.afterJob (fun x ->
        let now = StopwatchTicks.getTimestamp ()
        onComplete (Gauge.ofStopwatchTicks (now - ts))
        >>-. x)
      Job.start markNack >>-. altCommit)