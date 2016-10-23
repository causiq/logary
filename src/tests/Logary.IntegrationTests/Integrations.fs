module Integrations

open System
open System.Net
open NodaTime
open Expecto
open Hopac
open Logary
open Logary.Targets
open Logary.Internals

let env key =
  match Environment.GetEnvironmentVariable key with
  | null -> None
  | x -> Some x

let tryLookup (name : string) =
  try
    Choice1Of2 (Dns.GetHostEntry(name).AddressList.[0])
  with :? System.Net.Sockets.SocketException as e ->
    Choice2Of2 e

let parseIP str =
  match IPAddress.TryParse str with
  | false, _ ->
    match tryLookup str with
    | Choice1Of2 ip -> ip
    | Choice2Of2 e  -> raise (Exception (str, e))
  | true, ip -> ip

let ri =
  { serviceName = "tests"
    clock       = SystemClock.Instance
    logger      = NullLogger() }

[<Tests>]
let integration =
  let flush = Target.flush >> Job.Ignore >> run
  let stop = Target.shutdown >> Job.Ignore >> run
  let localhost = parseIP "localhost"

  testList "riemann" [
    testCase "sending metrics to riemann" <| fun _ ->
      let riemann =
        Riemann.create
          (Riemann.RiemannConf.create(endpoint = IPEndPoint(localhost, 5555)))
          "riemann"
        |> Target.init ri
        |> run

      try
        Message.gauge (PointName.parse "Logary.Tests.Integration") (Float 14.7)
        |> Target.log riemann
        |> Job.Ignore
        |> run
        flush riemann
      finally
        stop riemann
    ]
