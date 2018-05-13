namespace Logary

open Hopac
open Hopac.Infixes

module Job =

  let timeFun onComplete xJ =
    Job.delay (fun () ->
      let ts = StopwatchTicks.getTimestamp()
      xJ >>- fun x ->
      let now = StopwatchTicks.getTimestamp()
      onComplete (Gauge.ofStopwatchTicks (now - ts))
      x)

  let timeJob onCompleteJ xJ =
    Job.delay (fun () ->
      let ts = StopwatchTicks.getTimestamp()
      xJ >>= fun x ->
      let now = StopwatchTicks.getTimestamp()
      onCompleteJ (Gauge.ofStopwatchTicks (now - ts)) >>- fun () ->
      x)