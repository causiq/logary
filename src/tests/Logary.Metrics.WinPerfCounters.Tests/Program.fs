module Program

open Expecto
open Logary
open Logary.Metrics

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
      WinPerfCounter.Helpers.pidInstance () |> ignore

    testCase "getting value from perf counter" <| fun _ ->
      let apps = WinPerfCounters.appCounters ()
      apps.[0].nextValues () |> ignore
  ]

[<EntryPoint>]
let main argv =
  Tests.runTestsInAssembly defaultConfig argv
