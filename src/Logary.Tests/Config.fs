module Logary.Tests.Config

open Swensen.Unquote
open Fuchu

open System
open System.IO
open System.Text.RegularExpressions

open Logary
open Logary.Measure
open Logary.Formatting
open Logary.Logging
open Logary.Configuration.Config
open Logary.Internals
open Logary.Internals.Tcp

open TestDSL

let isTarget name (t : Target.TargetInstance) = t.name =? name

open Logary.Targets.TextWriter

[<Tests>]
let tests =
  testList "Config" [

    testCase "logary lifecycle" <| fun _ ->
      confLogary "tests"
      |> validateLogary
      |> runLogary
      |> shutdownLogary
      |> Async.RunSynchronously
      |> ignore

    testCase "target lifecycle" <| fun _ ->
      Target.confTarget "tw" (create (TextWriterConf.Default(Fac.textWriter(), Fac.textWriter())))
      |> Target.validateTarget
      |> Target.initTarget { serviceName = "tests"; logger = NullLogger() }
      |> Target.send (LogLine.debug "Hello")
      |> Target.shutdownTarget
      |> Async.RunSynchronously
      |> ignore
    ]
