module Program

open Fuchu

[<EntryPoint>]
let main args = //defaultMainThisAssembly args
  Logary.Tests.Internals.maps
  |> Tests.run