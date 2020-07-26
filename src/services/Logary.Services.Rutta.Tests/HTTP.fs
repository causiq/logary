module Logary.Services.Rutta.Tests.HTTP

open System.Threading
open Expecto
open Expecto.Flip
open Hopac
open HttpFs
open Jaeger.Thrift
open Logary
open Logary.Services.Rutta

[<Tests>]
let tests =
  testList "http" [
    testJob "spawn, get event message, shut down" {
      use slim = new ManualResetEventSlim(false)
      let args = [| "router"; "--target"; "console://." |]
      let! p = Proc.queue (Job.Scheduler.isolate <| fun () -> Program.execute args slim |> ignore)
      return ()
    }
  ]
  |> testLabel "rutta"
  |> testLabel "logary"