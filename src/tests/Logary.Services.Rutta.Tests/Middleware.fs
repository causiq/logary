module Logary.Services.Rutta.Tests.Middleware

open ExpectoPatronum
open Fuchu
open Logary
open Logary.Services.Rutta
open Logary.Services.Rutta.Program

[<Tests>]
let messageEnrichment =
  testList "Enrichment" [
    testCase "message has host" <| fun _ ->
      let hostname = System.Net.Dns.GetHostName()
      
      let msg = Logary.Message.event LogLevel.Info "Info" 
      let enrichedMsg = Middleware.host id msg

      Expect.isTrue (enrichedMsg.context |> Map.containsKey "host") "Has 'host' key"
      Expect.equal (Value.String hostname)
                   (enrichedMsg.context |> Map.find "host")
                   "Should contain the 'host' value and it should equal the Dns.GetHostName() value"
  ]

