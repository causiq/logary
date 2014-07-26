module Logary.Tests.ExternalAssumptions

#if INTERACTIVE
#r "bin/Release/Intelliplan.Logary.dll"
#endif

open Fuchu

open Logary
open Logary.Configuration

let expected_level = Warn

let configure_level (r : Rule) =
  { r with level = Some Warn |> Option.fold (fun s t -> t) Debug }

let givens () = goodDefaults "tests" |> fun conf -> { conf with rules = conf.rules |> List.map configure_level }

[<Tests>]
let ``retrieving rule for name`` () =
  testCase "all should be warn" <| fun _ ->
    let lconf = givens ()
    lconf |> validate |> runLogary |> shutdown |> Async.RunSynchronously |> ignore
    Assert.Equal("all levels should be Warn",
                 lconf.rules |> List.fold (fun acc r -> r.level = Warn && acc) true, true)
