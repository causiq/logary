module Logary.Tests.Config

open Swensen.Unquote
open Fuchu

open System
open System.IO
open System.Text.RegularExpressions

open Logary
open Logary.Target
open Logary.Targets
open Logary.Measure
open Logary.Formatting
open Logary.Logging
open Logary.Configuration.Config
open Logary.Internals
open Logary.Internals.Tcp

open TestDSL

let isTarget name (t : TargetInstance) = t.name =? name

open TextWriter

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
      confTarget "tw" (create (TextWriterConf.Default(Fac.textWriter(), Fac.textWriter())))
      |> validateTarget
      |> initTarget { serviceName = "tests"; logger = NullLogger() }
      |> send (LogLine.debug "Hello")
      |> shutdownTarget
      |> Async.RunSynchronously
      |> ignore
    ]
