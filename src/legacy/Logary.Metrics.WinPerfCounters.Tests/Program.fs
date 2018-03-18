module Program

open Expecto
open System
open Logary
open Logary.Metrics

let onMono () =
  Type.GetType "Mono.Runtime" <> null

[<Tests>]
let tests =
  testList "performance counters" [
    testList "point names" [
      testCase "from WinPerfCounter" <| fun _ ->
        let wpc = WinPerfCounter.create("A", "B", [])
        let subject = PointName.ofPerfCounter wpc
        Expect.equal subject (PointName [| "A"; "B" |])
                     "Should just concatenate the names"

      testCase "from WinPerfCounter with instance" <| fun _ ->
        let wpc = WinPerfCounter.create("A", "B", ["C"])
        let subject = PointName.ofPerfCounter wpc
        Expect.equal subject (PointName [| "A"; "B" |])
                     "Should not include the instance name"

      testCase "has replaced dots with dashes" <| fun _ ->
        let wpc = WinPerfCounter.create ("A.X", "B.Y", ["C.Z"])
        let subject = PointName.ofPerfCounter wpc
        Expect.equal subject (PointName [| "A-X"; "B-Y" |])
                     "Should have replaced dots with dashes"
    ]

    testCase "can get pid instance" <| fun _ ->
      if onMono () then Tests.skiptest "Helpers.getPidinstance fails on mono"
      WinPerfCounter.Helpers.pidInstance () |> ignore

  ]

[<EntryPoint>]
let main argv =
  Tests.runTestsInAssembly defaultConfig argv
