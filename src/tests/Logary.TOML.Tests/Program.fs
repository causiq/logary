module Program

open System.Reflection

open Fuchu
open Fuchu.Helpers
open Fuchu.Impl

/// Runs tests in this assembly with supplied command-line options.
/// You may also pass a filter that selected a subset of tests to run.
/// Returns 0 if all tests passed, otherwise 1
[<CompiledNameAttribute("DefaultMainThisAssembly")>]
let defaultMainThisAssemblyFilter args filter =
  let tests =
    match testFromAssembly (Assembly.GetEntryAssembly()) with
    | Some t -> filter t
    | None -> TestList []
  defaultMain tests args

let rec filterTests strs =
  let filter = function
               | TestLabel (label, t) -> Seq.fold (fun s t -> not (label.Contains(t)) && s) true strs
               | _ -> true
  function
  | TestList tl -> TestList (Seq.filter filter tl)
  | x -> x

[<EntryPoint>]
let main args =
  Logary.Tests.Parsing.basicExample |> Tests.run