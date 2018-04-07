module Logary.Targets.InfluxDb.Tests.TargetTests

open NodaTime
open System
open System.Text
open System.Threading
open Logary
open Logary.Tests.Utils
open Logary.Configuration
open Logary.Internals
open Logary.Targets.InfluxDb
open Expecto
open Expecto.Flip
open Suave
open Suave.Operators
open Hopac
open Hopac.Extensions
open Hopac.Infixes

let start (port: int) =
  let uri = Uri (sprintf "http://127.0.0.1:%i/write" port)
  let targConf = InfluxDbConf.create(uri, "tests")
  emptyRuntime >>= (fun (ri, ilogger) ->
  Target.create ri (create targConf "influxdb"))

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
      printfn "GOT REQ"
      do! Job.toAsync (Ch.give state.req r)
      return! Successful.NO_CONTENT ctx
    }))

  Async.Start(srv, cts.Token)
  Job.fromAsync (Async.Ignore listening) >>-.
  state

let testCaseTarget name fn =
  testCaseJob name (job {
    use! state = withServer ()
    let! target = start state.port
    do! Job.tryFinallyJob (fn state target) (shutdown target)
  })

[<Tests>]
let writesOverHttp =
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

  testList "writes over HTTP" [
    testCaseTarget "write single" (fun state target ->
      job {
        let! ack = Target.log target msg
        let! req = Ch.take state.req
        do! ack

        let expected = Serialise.message msg

        Encoding.UTF8.GetString req.rawForm
          |> Expect.equal "Should serialise correctly" expected

        req.queryParam "db"
          |> Expect.equal "Should write to tests db" (Choice1Of2 "tests")
      })

    testCaseTarget "write batch" (fun state target ->
      job {
        let! p1 = Target.log target msg1
        let! p2 = Target.log target msg2
        let! p3 = Target.log target msg3
        let! req = Ch.take state.req
        let! req2 = Ch.take state.req

        Encoding.UTF8.GetString req2.rawForm
          |> Expect.equal
              "Should newline-concatenate messages"
              (sprintf "%O\n%O" (Serialise.message msg2) (Serialise.message msg3))

        req.queryParam "db"
          |> Expect.equal "Should write to tests db" (Choice1Of2 "tests")
      })

    testCaseTarget "target acks" (fun state target ->
      let msg = Message.gaugefs "S1" "Number 1" 0.3463
      job {
        let! ackPromise = Target.log target msg
        let! req2 = Ch.take state.req

        let! success =
          Alt.choose [
            ackPromise ^->. true
            timeOut (TimeSpan.FromMilliseconds 8000.0) ^->. false // see msg if you change timeout
          ]

        success |> Expect.isTrue "Message should be acked, but failed to get acked within eight seconds."
      })
  ]
  |> testLabel "influxdb"