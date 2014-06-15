module Logary.Tests.HealthChecks

open Fuchu
open Swensen.Unquote

open System.Text.RegularExpressions

open Fac

open Logary
open Logary.Configuration
open Logary.Target
open Logary.Targets
open Logary.HealthChecks
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
  async {
    use p = new Ping()
    let awaitPong = Async.AwaitEvent(p.PingCompleted, p.SendAsyncCancel)
    try
      p.SendAsync("svd.se", 1000, obj())
      let! complete = awaitPong
      if complete.Cancelled then
        return Unhealthy <| Failure.Create("Ping was cancelled")
      elif not (complete.Error = null) then
        return Unhealthy <| Failure.Create("Ping got error", Some <| complete.Error)
      else return Healthy
    with e ->
      return Unhealthy <| Failure.Create("Exception when pinging haf.se", Some(e)) }

let tests =
  testList "health checks" [
    testCase "creating primitive check" <| fun _ ->
      let a () = async { return Unhealthy <| Failure.Create("Bad bad bad") }
      let h = fromFn "a" a
      let gotUnhealthy = untilPred 1000 (fun i -> match h.Check () with Unhealthy _ -> true | _ -> false)
      match h.Check () with
      | Unhealthy res -> res.Message =? "Bad bad bad"
      | _ -> failwith "expected unhealthy result"

    testCase "real ping" <| fun _ ->
      Fuchu.Tests.skiptest "does network IO"
      let h = fromFn "ping haf.se" pingSvdSe
      let gotUnhealthy = untilPred 10000 <| fun i -> match h.Check () with Unhealthy _ -> true | _ -> false
      gotUnhealthy =? false
      //let gotHealthy = untilPred 10000 <| fun i -> h.Check = Healthy
      //gotHealthy =? true
    ]