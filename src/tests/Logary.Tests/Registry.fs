module Logary.Tests.Registry

open System
open System.IO
open System.Text.RegularExpressions
open Expecto
open Hopac
open Hopac.Infixes
open TestDSL
open Logary
open Logary.Targets
open Logary.Configuration
open Logary.Tests

// framework API tests

[<Tests>]
let registry =
  testList "Registry" [
    yield testCase "getLogger" <| fun _ ->
      Fac.withLogary <| fun logary out err ->
        let logger = pnp "a.b.c.d" |> Registry.getLogger logary.registry |> run
        let logger' = pnp "a.b.c.d" |> (logary |> asLogManager).getLogger
        (because "logging normally" <| fun () ->
          Message.eventInfo "Hello world" |> Logger.log logger |> run
          Message.eventFatal "Goodbye cruel world" |> Logger.log logger' |> run
          logary |> Fac.finaliseLogary
          out.ToString(), err.ToString())
        |> theTuple
          (fun first second ->
            first |> should contain "Hello world" |> thatsIt
            second |> should contain "Goodbye cruel world" |> thatsIt)
        |> thatsIt

    yield testCase "after shutting down no logging happens" <| fun _ ->
      Fac.withLogary <| fun logary out err ->
        let logger = (pnp "a.b.c.d") |> Registry.getLogger logary.registry |> run
        (because "logging something, then shutting down" <| fun () ->
          // log and wait for Message to be flushed
          Message.eventInfo "hi there" |> Logger.logWithAck logger |> run |> run
          logary |> Config.shutdownSimple |> run |> ignore
          // this will place the info message in the buffers, but since the target is not
          // draining its buffer/queue it won't be logged
          Message.eventInfo "after shutdown" |> Logger.log logger |> run
          out.ToString())
        |> should contain "hi there"
        |> shouldNot contain "after shutdown"
        |> thatsIt
    ]

[<Tests>]
let registryMid =
  testList "registry and middleware" [
    yield testCase "logger uses middleware" <| fun _ ->
      let out, err =
        Fac.textWriter(), Fac.textWriter()
      let tw =
        TextWriter.TextWriterConf.create(out, err, Formatting.JsonFormatter.Default)

      let logary =
        confLogary "my service"
        |> withRule (Rule.createForTarget "tw")
        |> withTarget (Target.confTarget "tw" (TextWriter.create tw))
        |> withMiddleware (fun next msg ->
          msg |> Message.setContext "service" "my service" |> next)
        |> withMiddleware (fun next msg ->
          msg |> Message.setContext "hostname" "localhost" |> next)
        |> withMiddleware (fun next msg ->
          msg |> Message.setContext "messageId" "theMessageId" |> next)
        |> validate
        |> withInternalTarget LogLevel.Fatal (Console.create Console.empty "internal")
        |> runLogary
        |> run

      let logger = (pnp "a.b.c.d") |> Registry.getLogger logary.registry |> run

      Message.eventError "User clicked wrong button" |> Logger.log logger |> run

      logary |> Fac.finaliseLogary

      
      Expect.stringContains (err.ToString())
                            "\"service\":\"my service\""
                            "should have context 'service' key"
      
      Expect.stringContains (err.ToString())
                            "\"hostname\":\"localhost\""
                            "should have context 'hostname' key"

      Expect.stringContains (err.ToString())
                            "\"messageId\":\"theMessageId\""
                            "should have context 'messageId' key"

    yield testCase "logger uses middleware supplied when getting logger" <| fun _ ->
      
      let middleware next msg =
        msg |> Message.setContext "getLogger" "inserted" |> next 

      Fac.withLogary <| fun logary out err ->
        let logger = (pnp "a.b.c.d") |> Registry.getLoggerWithMiddleware middleware logary.registry |> run
        Message.eventError "User clicked wrong button" |> Logger.log logger |> run
        logary |> Fac.finaliseLogary

      
        Expect.stringContains (err.ToString())
                              "\"getLogger\":\"inserted\""
                              "should have context 'getLogger' key"
  ]
