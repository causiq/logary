module Program

open System
open NodaTime
open Expecto
open Hopac
open Hopac.Infixes
open Logary.Utils.Chiron
open Logary
open Logary.Target
open Logary.Targets
open Logary.Internals

let emptyRuntime = RuntimeInfo.create "tests"

let flush = Target.flush >> Job.Ignore >> run

let targConf =
  Logstash.LogstashConf.create()

let start () =
  Target.init emptyRuntime (Logstash.create targConf "influxdb")
  |> run
  |> fun inst -> inst.server (fun _ -> Job.result ()) None |> start; inst

let finaliseTarget = Target.shutdown >> fun a ->
  a ^-> TimeoutResult.Success <|> timeOutMillis 1000 ^->. TimedOut
  |> run
  |> function
  | TimedOut -> Tests.failtest "finalising target timeout"
  | TimeoutResult.Success _ -> ()

let raisedExn msg =
  let e = ref None : exn option ref
  try raise <| ApplicationException(msg)
  with ex -> e := Some ex
  (!e).Value

let now = Message.setUTCTicks System.DateTime.UtcNow.Ticks

[<Tests>]
let target =
  testList "logstash" [
    testCase "start and stop" <| fun _ ->
      let target = Logstash.create targConf "logstash-integration"
      let subject = target |> init emptyRuntime |> run
      Message.eventWarn "integration test" |> Target.log subject |> run |> run
      subject |> finaliseTarget

    testCase "serialise" <| fun _ ->
      let e1 = raisedExn "darn"
      let e2 = raisedExn "actual exn"

      let subject = 
        Message.eventWarn "Testing started"
        |> Message.setName (PointName.ofArray [| "a"; "b"; "c" |])
        |> Message.setField "data-key" "data-value"
        |> Message.setField "tags" [ "integration" ]
        |> Message.setField "e" e1
        |> Message.setContext "service" "tests"
        |> Message.addExn e2
        |> now
        |> Logstash.serialise

      let expected =
        [ "@timestamp", Json.String ""
          "@version", Json.String "1"
          "level", Json.String "warn"
          "context", Json.Object ([ "service", Json.String "tests" ] |> Map.ofList)
         ] |> Map.ofList |> Json.Object

      Expect.equal subject expected "should serialise to proper message"
    ]
