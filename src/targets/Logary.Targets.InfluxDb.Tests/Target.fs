module Logary.Targets.InfluxDb.Tests.TargetTests

open Logary
open Logary.Configuration
open Logary.Internals
open Logary.Targets.InfluxDb
open System
open System.Text
open System.Threading
open Fuchu
open Suave
open Suave.Operators
open Hopac
open Hopac.Infixes

let emptyRuntime = { serviceName = "tests"; logger = NullLogger() }

let flush = Target.flush >> Job.Ignore >> Job.Global.run

let start () =
  let targConf =
    InfluxDbConf.create(Uri "http://127.0.0.1:9011/write", "tests")

  Target.init emptyRuntime (create targConf (PointName.ofSingle "influxdb"))
  |> run
  |> fun inst -> inst.server |> start; inst

let finaliseTarget = Target.shutdown >> fun a ->
  a ^-> TimeoutResult.Success <|> timeOutMillis 1000 ^->. TimedOut
  |> Job.Global.run
  |> function
  | TimedOut -> Tests.failtest "finalising target timeout"
  | TimeoutResult.Success _ -> ()

type State(cts : CancellationTokenSource) =
  let request = IVar ()

  member x.req = request

  interface IDisposable with
    member x.Dispose () =
      cts.Cancel()
      cts.Dispose()

[<Tests>]
let writesOverHttp =

  let withServer () =
    let cts = new CancellationTokenSource()
    let state = new State(cts)
    let cfg =
      { defaultConfig with bindings = [ HttpBinding.mkSimple HTTP "127.0.0.1" 9011 ] }
    let listening, srv =
      startWebServerAsync cfg (request (fun r ->
        IVar.fill state.req r |> run
        Successful.NO_CONTENT))
    Async.Start srv
    listening |> Async.Ignore |> Async.RunSynchronously
    state

  testList "writes over HTTP" [
    testCase "write to Suave" <| fun _ ->
      let msg = Message.metric (PointName.parse "Processor.% User Time._Total") (Float 0.3463)
      use state = withServer ()

      let target = start ()
      try
        msg
        |> Target.log target
        |> run
        |> run

        let req =
          IVar.read state.req
          |> run

        Assert.equal (req.rawForm |> Encoding.UTF8.GetString)
                     (Serialisation.serialiseMessage msg)
                     "should eq"

        Assert.equal (req.queryParam "db") (Choice1Of2 "tests") "should write to tests db"
      finally
        finaliseTarget target
  ]