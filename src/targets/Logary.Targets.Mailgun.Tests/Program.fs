module Logary.Targets.Mailgun.Tests.Program

open System
open System.Net.Mail
open Mailgun.Api
open Fuchu
open Logary
open Logary.Internals
open Logary.Targets.Mailgun

let emptyRuntime = { serviceName = "tests"; logger = NullLogger() }

let flush = Target.flush >> Async.Ignore >> Async.RunSynchronously

let stop = Target.shutdown >> Async.Ignore >> Async.RunSynchronously

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

  Target.init emptyRuntime (create conf "mailgun")

[<Tests>]
let tests =
  testList "giving it a spin" [
    testCase "initialise" <| fun _ ->
      stop (start ())

    testCase "can send test e-mail" <| fun _ ->
      let target = start ()
      try
        (LogLine.error "hello world") |> Target.sendLogLine target
        flush target
      finally stop target

  ]

[<EntryPoint>]
let main argv =
  Tests.defaultMainThisAssembly argv