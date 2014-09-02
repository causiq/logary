module Logary.Tests.Logentries

open Fuchu

open Logary
open Logary.Internals
open Logary.Targets.Logentries

let emptyRuntime = { serviceName = "tests"; logger = NullLogger() }

[<Tests>]
let targetTests =
  let flush = Target.flush >> Async.Ignore >> Async.RunSynchronously

  let stop = Target.shutdown >> Async.Ignore >> Async.RunSynchronously

  let start () =
    // This are my test account token, for a demo host
    let conf =
      { empty with
          token = "ffe78bc6-fc2e-448f-9c81-fadd90dc06c1" }
    Target.init emptyRuntime (create conf "logentries")

  testList "logentries target" [
    testCase "smoke" <| fun _ ->
      ()

    testCase "initialise" <| fun _ ->
      stop (start ())

    testCase "initialise and log" <| fun _ ->
      let target = start ()
      try
        (LogLine.info "hello world") |> Target.sendLogLine target
        flush target
      finally stop target
    ]