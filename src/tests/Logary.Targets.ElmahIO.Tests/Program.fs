module Program

open Fuchu
open Hopac
open Logary
open Logary.Targets

[<Tests>]
let tests =
  testList "elmah.io tests" [
    testCase "basic" <| fun _ ->
      Logary.Tests.Targets.basicTests (ElmahIO.create ElmahIO.empty "elmahio")

    testCase "integration" <| fun _ ->
      Logary.Tests.Targets.integrationTests (ElmahIO.create ElmahIO.empty "elmahio")
  ]

[<EntryPoint>]
let main argv =
  Tests.defaultMainThisAssembly argv