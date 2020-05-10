module Bicycle.SupervisorProcess
open Logary
open Logary.Message

// "the sensor"
let logger = Log.create "Car.SupervisorProcess"

let tyreTick () =
  // "the event is triggered, e.g. via a polling supervisor"
  logger.info (
    eventX "Tyres"
    >> addGauge "front" (Gauge (Float 79.2421, Units.Pascal)))
    >> addGauge "back" (Gauge (Float 90.159, Units.Pascal)))