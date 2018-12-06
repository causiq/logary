module Logary.Targets.InfluxDb.Tests.TargetTests

open NodaTime
open System
open System.Text
open System.Threading
open Expecto
open Expecto.Flip
open Logary
open Logary.Tests
open Logary.Configuration
open Logary.Internals
open Logary.Targets.InfluxDb
open Suave
open Suave.Operators
open Hopac
open Hopac.Extensions
open Hopac.Infixes

let start factory (port: int) =
  let uri = Uri (sprintf "http://127.0.0.1:%i/write" port)
  let targConf = factory (InfluxDbConf.create(uri, "tests"))
  emptyRuntime >>= fun ri ->
  Target.create ri (create targConf "influxdb")

let shutdown t =
  job {
    let! ack = Target.shutdown t
    do! ack
  }

type State(cts: CancellationTokenSource, port: int) =
  let request = Ch ()
  member x.req = request
  member x.port = port
  interface IDisposable with
    member x.Dispose () =
      cts.Cancel()
      cts.Dispose()

let mutable port = 9011

let withServer () =
  let cts = new CancellationTokenSource()
  let state = new State(cts, Interlocked.Increment(&port))
  let cfg =
    let binding = HttpBinding.createSimple HTTP "127.0.0.1" state.port
    { defaultConfig with
        bindings = [ binding ]
        cancellationToken = cts.Token }

  let listening, srv =
    startWebServerAsync cfg (request (fun r ctx -> async {
      //printfn "GOT REQ"
      do! Job.toAsync (Ch.give state.req r)
      return! Successful.NO_CONTENT ctx
    }))

  Async.Start(srv, cts.Token)
  Job.fromAsync (Async.Ignore listening) >>-.
  state

let testCaseTarget factory name fn =
  testCaseJob name (job {
    use! state = withServer ()
    let! target = start factory state.port
    do! Job.tryFinallyJob (fn state target) (shutdown target)
  })

let disableCreateDB (conf: InfluxDbConf) = { conf with disableCreateDB = true }

[<Tests>]
let tests =
  let msg =
    Message.gaugeWithUnitfs "Processor" "% User Time" Percent 1.
    |> Message.setField "inst1" 0.3463
    |> Message.setField "inst2" 0.223
    |> Message.setContext "service" "svc-2"
    |> Message.tag "my-tag"
    |> Message.tag "ext"

  let msg1 = Message.gaugefs "Processor" "Number 1" 0.3463
  let msg2 = Message.gaugefs "Processor" "Number 2" 0.3463
  let msg3 = Message.gaugefs "Processor" "Number 3" 0.3463

  testList "influxdb" [
    testList "writes over HTTP" [
      testCaseTarget disableCreateDB "write single" (fun state target ->
        job {
          let! ack = Target.tryLog target msg ^-> Expect.isOk "Successfully placed message in buffer"
          let! req = Ch.take state.req
          do! ack

          let expected = Serialise.message Constants.AllowedInfluxTags msg

          Encoding.UTF8.GetString req.rawForm
            |> Expect.equal "Should serialise correctly" expected

          req.queryParam "db"
            |> Expect.equal "Should write to tests db" (Choice1Of2 "tests")
        })

      testCaseTarget disableCreateDB "write batch" (fun state target ->
        job {
          let! p1 = Target.tryLog target msg1 ^-> Expect.isOk "Successfully placed message in buffer"
          let! p2 = Target.tryLog target msg2 ^-> Expect.isOk "Successfully placed message in buffer"
          let! p3 = Target.tryLog target msg3 ^-> Expect.isOk "Successfully placed message in buffer"
          let! req = Ch.take state.req
          let! req2 = Ch.take state.req

          Encoding.UTF8.GetString req2.rawForm
            |> Expect.equal
                "Should newline-concatenate messages"
                (sprintf "%O\n%O" (Serialise.message Set.empty msg2) (Serialise.message Set.empty msg3))

          req.queryParam "db"
            |> Expect.equal "Should write to tests db" (Choice1Of2 "tests")
        })

      testCaseTarget disableCreateDB "target acks" (fun state target ->
        let msg = Message.gaugefs "S1" "Number 1" 0.3463
        job {
          let! ackPromise = Target.tryLog target msg ^-> Expect.isOk "Successfully placed message in buffer"
          let! req2 = Ch.take state.req

          let! success =
            Alt.choose [
              ackPromise ^->. true
              timeOut (TimeSpan.FromMilliseconds 8000.0) ^->. false // see msg if you change timeout
            ]

          success |> Expect.isTrue "Message should be acked, but failed to get acked within eight seconds."
        })
    ]

    testCaseTarget id "create database if missing" <| fun state target ->
      Job.result ()
  ]
