module Logary.Services.Rutta.Tests.Middleware

open Expecto
open Logary
open Logary.Services.Rutta
open Logary.Services.Rutta.Program

[<Tests>]
let messageEnrichment =
  testList "Enrichment" [
    testCase "message has host" <| fun _ ->
      let hostname = System.Net.Dns.GetHostName()
      
      let msg = Logary.Message.event LogLevel.Info "Info" 
      let enrichedMsg = Middleware.host hostname id msg

      Expect.equal (Some hostname)
                   (enrichedMsg |> Message.tryGetContext KnownLiterals.HostContextName)
                   "Should contain the 'host' value and it should equal the Dns.GetHostName() value"
  ]
