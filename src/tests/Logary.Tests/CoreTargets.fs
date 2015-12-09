module Logary.Tests.CoreTargets

open Fuchu
open Swensen.Unquote

open Logary
open Logary.Targets.TextWriter

open TestDSL

open Fac

let tests =
  testList "CoreTargets" [
    testCase "initialising TextWriter target" <| fun _ ->
      let target = create (TextWriterConf.Create(System.Console.Out, System.Console.Error)) (PointName.ofSingle "sample console")
      let instance = target.initer emptyRuntime
      instance.name =? PointName.ofSingle "sample console"

    testCase "writing with Console target directly" <| fun _ ->
      let stdout = Fac.textWriter ()
      let target = create (TextWriterConf.Create(stdout, stdout)) (PointName.ofSingle "writing console target")
      let instance = target |> Target.init emptyRuntime

      (because "logging with info level and then finalising the target" <| fun () ->
        "Hello World!" |> Message.info |> Target.sendMessage instance
        instance |> finaliseTarget
        stdout.ToString())
      |> should contain "Hello World!"
      |> thatsIt

    testCase "``error levels should be to error text writer``" <| fun _ ->
      let out, err = Fac.textWriter (), Fac.textWriter ()
      let target = create (TextWriterConf.Create(out, err)) (PointName.ofSingle "error writing")
      let subject = target |> Target.init emptyRuntime

      (because "logging 'Error line' and 'Fatal line' to the target" <| fun () ->
        Message.error "Error line" |> Target.sendMessage subject
        Message.fatal "Fatal line" |> Target.sendMessage subject
        subject |> finaliseTarget
        err.ToString())
      |> should contain "Error line"
      |> should contain "Fatal line"
      |> thatsIt
    ]