module Logary.Targets.Mailgun.Tests.Program

open Fuchu
open NodaTime
open System
open System.Net.Mail
open Hopac
open Hopac.Infixes
open Mailgun.Api
open Logary
open Logary.Internals
open Logary.Targets.Mailgun

let emptyRuntime =
  { serviceName = "tests"
    clock       = SystemClock.Instance
    logger      = NullLogger() }

let flush = Target.flush >> Job.Ignore >> run

let env k =
  match Environment.GetEnvironmentVariable k with
  | null -> failwithf "couldn't load key %s" k
  | v -> v

let start () =
  let getOpts (domain, line) = { SendOpts.Create domain with testMode = true }

  // You'll have to set the environment var MAILGUN_API_KEY to run this;
  // but besides that, this is the only configuration you need:
  let conf = MailgunLogaryConf.Create(MailAddress "hi@haf.se",
                                      [ MailAddress "doesntexist@haf.se" ],
                                      { apiKey = env "MAILGUN_API_KEY" },
                                      "haf.se",
                                      getOpts = getOpts)

  Target.init emptyRuntime (create conf (PointName.ofSingle "mailgun"))
  |> run
  |> fun inst -> inst.server |> start; inst

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

    testCase "can send test e-mail" <| fun _ ->
      let target = start ()
      try
        Message.eventError "hello world"
        |> Target.log target
        |> run
        |> run

        target |> flush
      finally finaliseTarget target
  ]

[<EntryPoint>]
let main argv =
  Tests.defaultMainThisAssembly argv