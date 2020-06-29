module Logary.Tests.TargetBaseline

open System.Collections.Generic
open Logary
open Logary.Tests.Utils

module Messages =

  type Sweetness =
    | Cake of slices:uint16
    | NoCake
    | FruitSalad

    interface IValueFormattable with
      member x.toKeyValues(baseKey: string) =
        let value = match x with
                    | Cake slices -> Value.Str (sprintf "Cake with %i slices" slices)
                    | NoCake -> Value.Str "No cake"
                    | FruitSalad -> Value.Str "Fruit salad"
        Choice1Of2 (KeyValuePair<_,_>(baseKey, value))

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
    let context =
      Map [
        "env", Value.Str "production"
        "service", Value.Str "WebApi"
        "machineType", Value.Str "n1-standard-1"
        "computeInstance", Value.Str "gke-project-id-default-pool-2de02f1c-6g3f"
        "tenant.id", Value.Int64 123L
        "tenant.name", Value.Str "Tom Petersson"
        "tenant.company", Value.Str "Org Biz"
      ]
      :> IReadOnlyDictionary<string, Value>

    let fields =
      Map [
        "userId", Value.Str "haf"
        "email", Value.Str "haf@example.com"
        "promoCount", Value.Int64 3L
        "ip", Value.Str "81.227.65.159" // 3 Sweden, Stockholm
      ]
      :> IReadOnlyDictionary<string, Value>

    let event =
      Model.Event("User#{userId} '{email}' signed up, after {promoCount} promotions: {@promotions}. Cake? {willThereBeCake}",
                  None, level=Info, ctx=context, fs=fields)

    event.tag "funnel"
    event.setField("afterDinner", FruitSalad)
    event.setField("willThereBeCake", Cake 12us)
    event.setField("promotions", [ "timeLimited2day"; "enableInvoices"; "friendReferral" ] |> String.concat ", ")
    event.setGauge("timeUntilSignup", Gauge (Value.Float 2.3, U.Days))
    event

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
      let! ri = emptyRuntime
      let! targetApi = Target.create ri conf
      let now = if addTS then SystemClock.Instance.GetCurrentInstant().ToString() else "-"
      return targetApi, now
    }

  testList (sprintf "target '%s' basics" targetName) [
    testCaseJob "create" <| job {
      let! ri = emptyRuntime
      do! logger.infoWithBP (eventX "Creating instance: calling configFactory")
      let conf = confFac targetName
      do! logger.infoWithBP (eventX "Creating instance: creating target")
      let! targetApi = Target.create ri conf
      do! logger.infoWithBP (eventX "Creating instance: asserting")
      Expect.equal targetApi.name targetName "Should be named"
      do! finalise targetApi
    }

    testCaseJob "start, log and stop" <| job {
      let! targetApi, now = configure ()
      do! logger.infoWithBP (eventX "Start, log and stop: log and wait")
      do! logMsgWaitAndShutdown targetApi (fun logAndWait ->
        Model.Event (sprintf "User signed up! @ %s" now) |> logAndWait)
      do! logger.infoWithBP (eventX "Start, log and stop: done!")
    }

    testCaseJob "log exception message" <| job {
      let! targetApi, now = configure ()
      let exnMsg = exnMsg.withEvent (sprintf "%s @ %s" exnMsg.event now)
      do! logMsgWaitAndShutdown targetApi (fun logAndWait -> logAndWait exnMsg)
    }

    testCaseJob "log user upgraded plan message" <| job {
      let! targetApi, now = configure ()
      do! logMsgWaitAndShutdown targetApi (fun logAndWait -> logAndWait Messages.userUpgradedPlan)
    }
  ]