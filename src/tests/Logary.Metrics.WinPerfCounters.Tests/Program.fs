module Program

open ExpectoPatronum
open Fuchu
open Logary
open Logary.Metrics

[<Tests>]
let tests =
  testList "performance counters" [
    testCase "PointName from WinPerfCounter" <| fun _ ->
      let wpc = WinPerfCounter.create("A", "B", [])
      let subject = PointName.ofPerfCounter wpc
      Expect.equal subject (PointName [| "A"; "B" |])
                   "Should just concatenate the names"

    testCase "PointName from WinPerfCounter with instance" <| fun _ ->
      let wpc = WinPerfCounter.create("A", "B", ["C"])
      let subject = PointName.ofPerfCounter wpc
      Expect.equal subject (PointName [| "A"; "B" |])
                   "Should not include the instance name"

    testCase "PointName has replaced dots with dashes" <| fun _ ->
      let wpc = WinPerfCounter.create ("A.X", "B.Y", ["C.Z"])
      let subject = PointName.ofPerfCounter wpc
      Expect.equal subject (PointName [| "A-X"; "B-Y" |])
                   "Should have replaced dots with dashes"
  ]

[<EntryPoint>]
let main argv = Tests.defaultMainThisAssembly argv