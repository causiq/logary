module Logary.Services.Rutta.Tests.MessageEnrichment

//open Fuchu
//open Logary
//open Logary.Services.Rutta
//open Logary.Services.Rutta.Shipper
//open Logary.Services.Rutta.Program
//
//[<Tests>]
//let messageEnrichment =
//   
//  testList "Enrichment" [
//    testCase "message has hostname" <| fun _ ->
//      let hostname = System.Net.Dns.GetHostName()
//      
//      let msg = Logary.Message.event LogLevel.Info "Info" 
//      let enrichedMsg = Shipper.enricher msg ""
//        
//      Assert.Equal("Should contain hostname key", true, enrichedMsg.context.ContainsKey("hostname"))
//      Assert.Equal("Should contain hostname value", Value.String hostname, enrichedMsg.context.Item("hostname"))      
//
//    testCase "message has servicename" <| fun _ ->
//      let serviceName = "the name of service"
//
//      let msg = Logary.Message.event LogLevel.Info "Info" 
//      let enrichedMsg = Shipper.enricher msg serviceName
//        
//      Assert.Equal("Should contain service key", true, enrichedMsg.context.ContainsKey("serviceName"))
//      Assert.Equal("Should contain hostname value", Value.String serviceName, enrichedMsg.context.Item("serviceName"))
//  ]
//
