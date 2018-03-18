module Datapoints

open Expecto

type Assert =
  static member Contains(msg: string, xExpected : 'a, xs : 'a seq) =
    match Seq.tryFind ((=) xExpected) xs with
    | None -> Tests.failtestf "%s -- expected %A to contain %A" msg xs xExpected
    | Some _ -> ()

open Logary
open Logary.Metrics

[<Tests>]
let datapoints =
  let dps =
    SQLServerHealth.Impl.dps "drive" "c"
    |> Set.ofList

  testList "getting all datapoints" [
    testCase "for drive c" <| fun _ ->
      Expect.sequenceEqual
        dps
        (["drive_io_stall_read.c"
          "drive_io_stall_write.c"
          "drive_io_stall.c"
          "drive_num_of_reads.c"
          "drive_num_of_writes.c"
          "drive_num_of_bytes_read.c"
          "drive_num_of_bytes_written.c"
          "drive_read_latency.c"
          "drive_write_latency.c"
          "drive_latency.c"
          "drive_bytes_per_read.c"
          "drive_bytes_per_write.c"
          "drive_bytes_per_transfer.c" ]
        |> List.map (sprintf "Logary.Metrics.SQLServerHealth.%s")
        |> List.map PointName.parse
        |> Set.ofList)
        "should eq set"
    ]
