module Logary.ExternalAssumptions

#if INTERACTIVE
#r "bin/Release/Intelliplan.Logary.dll"
#endif

open NUnit.Framework

open Logary
open Logary.Configuration.Config

let expected_level = Warn

let configure_level (r : Targets.Rule) =
  { r with level = Some Warn |> Option.fold (fun s t -> t) Debug }

let givens () = goodDefaults "tests" |> fun conf -> { conf with rules = conf.rules |> List.map configure_level }

[<Test>]
let ``retrieving rule for name`` () =
  let lconf = givens ()
  let running = lconf |> validateLogary |> runLogary
  Assert.IsTrue(lconf.rules |> List.fold (fun acc r -> r.level = Warn && acc) true, "all levels should be Warn")
