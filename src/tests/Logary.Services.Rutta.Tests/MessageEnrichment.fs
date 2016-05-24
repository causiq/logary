module Logary.Services.Rutta.Tests.MessageEnrichment

open Fuchu
open Logary
open Logary.Services.Rutta
open Logary.Services.Rutta.Program

[<Tests>]
let messageEnrichment =
   
  testList "Enrichment" [
    testCase "message has hostname" <| fun _ ->
      let hostname = System.Net.Dns.GetHostName()
      
      let msg = Logary.Message.event LogLevel.Info "Info" 
      let enrichedMsg = Logary.Services.Rutta.Middleware.addHostNameMiddleware id msg
        
      Assert.Equal("Should contain hostname key", true, enrichedMsg.context.ContainsKey("hostname"))
      Assert.Equal("Should contain hostname value", Value.String hostname, enrichedMsg.context.Item("hostname"))
  ]

