module Logary.Tests.CoreTargets

open Fuchu
open Logary
open Logary.Targets
open Logary.Tests.Targets
open Logary.Targets.TextWriter
open Hopac
open TestDSL
open Fac

let textWriterConf =
  TextWriterConf.create(System.Console.Out, System.Console.Error)

[<Tests>]
let tests =
  testList "CoreTargets" [
    Targets.basicTests "text writer" (TextWriter.create textWriterConf)
    Targets.integrationTests "text writer" (TextWriter.create textWriterConf)

    testCase "printing Hello World" <| fun _ ->
      let stdout = Fac.textWriter ()
      let target = create (TextWriterConf.create(stdout, stdout)) "writing console target"
      let instance = target |> Target.init emptyRuntime |> run
      instance.server (fun _ -> Job.result ()) None
      |> start

      (because "logging with info level and then finalising the target" <| fun () ->
        Message.eventInfo "Hello World!" |> Target.logAndWait instance
        Target.finalise instance
        stdout.ToString())
      |> should contain "Hello World!"
      |> thatsIt

    testCase "initialising TextWriter target" <| fun _ ->
      let stdout = Fac.textWriter ()
      let target = create (TextWriterConf.create(stdout, stdout)) "writing console target"
      let instance = target |> Target.init emptyRuntime |> run
      instance.server (fun _ -> Job.result ()) None
      |> start

      let x = dict ["foo", "bar"]
      (because "logging with fields then finalising the target" <| fun () ->
        Message.event Info "Hello World!" |> Message.setFieldFromObject "the Name" x |> Target.logAndWait instance
        Target.finalise instance
        stdout.ToString())
      |> should contain "Hello World!"
      |> should contain "foo"
      |> should contain "bar"
      |> should contain "the Name"
      |> thatsIt

    testCase "``error levels should be to error text writer``" <| fun _ ->
      let out, err = Fac.textWriter (), Fac.textWriter ()
      let target = create (TextWriterConf.create(out, err)) "error writing"
      let subject = target |> Target.init emptyRuntime |> run
      subject.server (fun _ -> Job.result ()) None
      |> start

      (because "logging 'Error line' and 'Fatal line' to the target" <| fun () ->
        Message.eventError "Error line" |> Target.logAndWait subject
        Message.eventFatal "Fatal line" |> Target.logAndWait subject
        Target.finalise subject
        err.ToString())
      |> should contain "Error line"
      |> should contain "Fatal line"
      |> thatsIt
    ]
