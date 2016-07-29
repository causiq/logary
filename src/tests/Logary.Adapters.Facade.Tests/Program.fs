module Program

open Fuchu
open Logary.Adapters.Facade

[<Tests>]
let tests =
  testList "facade" [
    testCase "create adapter" <| fun _ ->
      let instance = LogaryFacadeAdapter.create "Libryy.Logging" "Libryy"
      ()
  ]

[<EntryPoint>]
let main argv = 
  Tests.defaultMainThisAssembly argv