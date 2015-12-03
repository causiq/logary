module Integrations

open System
open System.Net
open Fuchu
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

let ri = { serviceName = "tests"; logger = NullLogger() }

[<Tests>]
let integration =
  let flush = Target.flush >> Async.Ignore >> Async.RunSynchronously
  let stop = Target.shutdown >> Async.Ignore >> Async.RunSynchronously
  let localhost = parseIP "localhost"

  testList "riemann" [
    testCase "sending metrics to riemann" <| fun _ ->
      let riemann =
        Riemann.create
          (Riemann.RiemannConf.Create(endpoint = IPEndPoint(localhost, 5555)))
          "riemann"
        |> Target.init ri
      try
        Measure.create' "logary.tests.integration" 14.7
        |> Target.sendMeasure riemann
        flush riemann
      finally
        stop riemann

    ]
