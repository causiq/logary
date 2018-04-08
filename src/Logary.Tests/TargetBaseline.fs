module Logary.Tests.TargetBaseline

open Logary
open Logary.Tests.Utils

module Messages =
  open Logary.Message

  /// A user signup event with lots of nice data:
  ///
  /// - Logged at Info level
  /// - A nice message template
  /// - Some context values (primitives)
  /// - A Map in the context
  /// - Some fields (to be templated into the string), including a string array
  /// - A single gauge with the # (float) of days the user waited until signing up.
  /// - A couple of tags
  ///
  let userUpgradedPlan =
    Message.event Info "User#{userId} '{email}' signed up, after {promoCount} promotions: {@promotions}."
    |> Message.setContexts [
      "env", box "production"
      "service", box "WebApi"
      "machineType", box "n1-standard-1"
      "computeInstance", box "gke-project-id-default-pool-2de02f1c-6g3f"
      "tenant", box (Map [ "id", box 123; "name", box "Company ABC" ])
    ]
    |> Message.setFieldsFromSeq [
      "userId", box "haf"
      "email", box "haf@example.com"
      "promoCount", box 3
      "ip", box "81.227.65.159" // 3 Sweden, Stockholm
    ]
    |> Message.tag "funnel"
    |> Message.setField "promotions" [ "timeLimited2day"; "enableInvoices"; "friendReferral" ]
    |> Message.addGauge "timeUntilSignup" (Gauge (Float 2.3, Units.Days))

open NodaTime
open Hopac
open Expecto
open Expecto.Logging
open Expecto.Logging.Message

let private logger = Log.create "Logary.Tests.TargetBaseline"

/// Run basic tests against the target;
///
///  - can create instance thereof
///  - can start and stop
///  - can receive a few different sorts of messages
///
let basicTests targetName confFac addTS =
  let configure () =
    job {
      let conf = confFac targetName
      let! ri, _ = emptyRuntime
      let! targetApi = Target.create ri conf
      let now = if addTS then SystemClock.Instance.GetCurrentInstant().ToString() else "-"
      return targetApi, now
    }

  testList (sprintf "target '%s' basics" targetName) [
    testCaseJob "create" <| job {
      let! ri, _ = emptyRuntime
      do! logger.infoWithBP (eventX "Creating instance: calling configFactory")
      let conf = confFac targetName
      do! logger.infoWithBP (eventX "Creating instance: creating target")
      let! targetApi = Target.create ri conf
      do! logger.infoWithBP (eventX "Creating instance: asserting")
      Expect.equal targetApi.Name targetName "Should be named"
    }

    testCaseJob "start, log and stop" <| job {
      let! targetApi, now = configure ()
      do! logger.infoWithBP (eventX "Start, log and stop: log and wait")
      do! logMsgWaitAndShutdown targetApi (fun logAndWait ->
        Message.eventInfo (sprintf "User signed up! @ %s" now) |> logAndWait)
      do! logger.infoWithBP (eventX "Start, log and stop: done!")
    }

    testCaseJob "log exception message" <| job {
      let! targetApi, now = configure ()
      let exnMsg = { exnMsg with value = sprintf "%s @ %s" exnMsg.value now }
      do! logMsgWaitAndShutdown targetApi (fun logAndWait -> logAndWait exnMsg)
    }

    testCaseJob "log user upgraded plan message" <| job {
      let! targetApi, now = configure ()
      do! logMsgWaitAndShutdown targetApi (fun logAndWait -> logAndWait Messages.userUpgradedPlan)
    }
  ]