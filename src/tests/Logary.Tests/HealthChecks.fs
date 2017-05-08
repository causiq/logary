﻿module Logary.Tests.HealthChecks

open Expecto
open System.Text.RegularExpressions
open Hopac
open Fac
open Logary
open Logary.HealthCheck
open Logary.Tests.TestDSL

open System.Net.NetworkInformation

let private untilPred maxWait fPred =
  let spy msg = let doubleSpy = printfn "%s: %A" msg in Seq.map doubleSpy
  let sleep = 20
  Seq.unfold (fun s -> if s < maxWait then Some(s+sleep, s+sleep) else printfn "n" ; None) 0
  |> Seq.map (fun x -> System.Threading.Thread.Sleep(sleep) ; x)
  //|> spy "sleep"
  |> Seq.skipWhile (not<<fPred)
  |> Seq.isEmpty
  |> not

let pingSvdSe () =
  let mkError ex =
    Message.gauge (PointName.ofList ["app";"resource";"ping-svd"]) (Float 0.0)
    |> setDesc "Ping completed with error."
    |> Message.addExn ex
    |> Message.setLevel LogLevel.Error
    |> HealthCheckResult.ofMessage
  job {
    use p = new Ping()
    let awaitPong = Async.AwaitEvent(p.PingCompleted, p.SendAsyncCancel)
    try
      p.SendAsync("svd.se", 1000, obj())
      let! complete = awaitPong
      if complete.Cancelled then
        return NoValue
      elif complete.Error <> null then
        return mkError complete.Error
      else
        return Message.gauge (PointName.ofList ["app";"resource";"ping-svd"])
                              (Float 1.0)
               |> Message.toHealthCheckResult

    with e ->
      return mkError e }

[<Tests>]
let tests =
  testList "health checks" [
    testCase "real ping" <| fun _ ->
      Tests.skiptest "does network IO"
      let h = fromFn (PointName.ofSingle "ping haf.se") pingSvdSe
      let gotUnhealthy = untilPred 10000 <| fun i ->
        match h.getValue () |> run with HasValue _ -> true | _ -> false
      Expect.isFalse gotUnhealthy "is not unhealthy"
    ]