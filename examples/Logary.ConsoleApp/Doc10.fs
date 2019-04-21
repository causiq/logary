namespace MyCompany.Sub

open Logary

type Worker() =
  let logger =
    Logging.getLoggerByName "MyCompany.Sub.Worker"

  let workName =
    PointName [| "MyCompany"; "Sub"; "Worker"; "workDone" |]

  let getAnswers (amount : float) =
    // work work work
    42 * amount

  member x.Work (amount : float) =
    // Initially, log how much work is to be done
    // the only "supported" metric is a gauge (a value at an instant)
    Message.gauge workName amount |> Logger.logSimple logger

    // do some work, logging how long it takes:
    let everything = Logger.time logger (fun () -> getAnswers amount)

    // return result
    everything