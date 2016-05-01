module Logary.Tests.CoreTargets

open Fuchu
open Swensen.Unquote
open Logary
open Logary.Targets.TextWriter
open Hopac
open TestDSL
open Fac

[<Tests>]
let tests =
  testList "CoreTargets" [
    testCase "initialising TextWriter target" <| fun _ ->
      let target = create (TextWriterConf.create(System.Console.Out, System.Console.Error)) (PointName.ofSingle "sample console")
      let instance = target.initer emptyRuntime |> run
      Assert.Equal("instance name should eq sample console", instance.name, PointName.ofSingle "sample console")
      start instance.server |> ignore

    testCase "writing with Console target directly" <| fun _ ->
      let stdout = Fac.textWriter ()
      let target = create (TextWriterConf.create(stdout, stdout)) (PointName.ofSingle "writing console target")
      let instance = target |> Target.init emptyRuntime |> run
      start instance.server |> ignore

      (because "logging with info level and then finalising the target" <| fun () ->
        Message.eventInfo "Hello World!" |> logTarget instance
        instance |> finaliseTarget
        stdout.ToString())
      |> should contain "Hello World!"
      |> thatsIt

    testCase "``error levels should be to error text writer``" <| fun _ ->
      let out, err = Fac.textWriter (), Fac.textWriter ()
      let target = create (TextWriterConf.create(out, err)) (PointName.ofSingle "error writing")
      let subject = target |> Target.init emptyRuntime |> run
      start subject.server |> ignore

      (because "logging 'Error line' and 'Fatal line' to the target" <| fun () ->
        Message.eventError "Error line" |> logTarget subject
        Message.eventFatal "Fatal line" |> logTarget subject
        subject |> finaliseTarget
        err.ToString())
      |> should contain "Error line"
      |> should contain "Fatal line"
      |> thatsIt
    ]