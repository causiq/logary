module Logary.Targets.Mixpanel.Tests.Program

open System
open System.Net.Mail
open Expecto
open Expecto.Flip
open NodaTime
open Hopac
open Hopac.Infixes
open Logary
open Logary.Internals
open Logary.Targets.Mixpanel

let emptyRuntime =
  { serviceName = "tests"
    host        = "nohost"
    clock       = SystemClock.Instance
    logger      = NullLogger() }

let flush = Target.flush >> Job.Ignore >> run

let env k =
  match Environment.GetEnvironmentVariable k with
  | null -> failwithf "couldn't load key %s" k
  | v -> v

let start () =
  // You'll have to set the environment var MIXPANEL_TOKEN to run this;
  // but besides that, this is the only configuration you need:
  let conf = MixpanelLogaryConf.create(env "MIXPANEL_TOKEN")

  Target.init emptyRuntime (create conf "mixpanel")
  |> run
  |> fun inst -> inst.server (fun _ -> Job.result ()) None |> start; inst

let finaliseTarget = Target.shutdown >> fun a ->
  a ^-> TimeoutResult.Success <|> timeOutMillis 1000 ^->. TimedOut
  |> run
  |> function
  | TimedOut -> Tests.failtest "finalising target timeout"
  | TimeoutResult.Success _ -> ()

[<Tests>]
let helloWorld =
  testList "giving it a spin" [
    testCase "initialise" <| fun _ ->
      finaliseTarget (start ())

    testCase "can send event" <| fun _ ->
      let target = start ()
      let user =
        Value.Object (Map ["id", Value.String "e13b16522932483c85c8db796864dedd"])
      try
        Message.eventError "hello mixpanel"
        |> Message.setContext "user" user
        |> Target.log target
        |> run
        |> run

        target |> flush
      finally finaliseTarget target
  ]

[<EntryPoint>]
let main argv =
  Tests.defaultMainThisAssembly argv