module Logary.Tests.HealthChecks

open Fuchu
open Swensen.Unquote

open System.Text.RegularExpressions

open Fac

open Logary
open Logary.HealthCheck
open Logary.Measure
open Logary.Internals.Tcp

open Logary.Tests.StubTcp
open Logary.Tests.TestDSL

open System.Net.NetworkInformation

let private untilPred maxWait fPred =
  let spy msg = let doubleSpy = printfn "%s: %A" msg in Seq.map doubleSpy
  let sleep = 20
  Seq.unfold (fun s -> if s < maxWait then Some(s+sleep, s+sleep) else printfn "n" ; None) 0
  |> Seq.map (fun x -> System.Threading.Thread.Sleep(sleep) ; x)
  |> spy "sleep"
  |> Seq.skipWhile (not<<fPred)
  |> Seq.isEmpty
  |> not

let pingSvdSe () =
  let mkError ex =
    mkMeasure "app.resource.ping-svd" 0.
               |> setDesc "ping completed with error"
               |> setExn ex
               |> setLevel Error
               |> Measure.toResult
  async {
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
        return mkMeasure "app.resource.ping-svd" 1. |> Measure.toResult
        
    with e ->
      return mkError e }

let tests =
  testList "health checks" [
    testCase "real ping" <| fun _ ->
      Fuchu.Tests.skiptest "does network IO"
      let h = fromFn "ping haf.se" pingSvdSe
      let gotUnhealthy = untilPred 10000 <| fun i ->
        match h.GetValue () with HasValue _ -> true | _ -> false
      gotUnhealthy =? false
    ]