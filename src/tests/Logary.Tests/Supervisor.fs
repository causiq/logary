module Logary.Tests.Supervisor

open Fuchu
open Logary
open Logary.Targets
open Logary.Targets.TextWriter
open Logary.Tests.Targets
open Logary.Supervisor
open Hopac
open Hopac.Infixes
open NodaTime
open ExpectoPatronum

let textWriterConf =
  TextWriterConf.create(System.Console.Out, System.Console.Error)

let internalLogger = NullLogger()

[<Tests>]
let supervisorTests =
  testList "Tests of basic supervisor functionality" [
    testCase "Supervisor starts targets" <| fun _ ->
      let sup = create internalLogger
      let target =
        Target.confTarget "supervised" (TextWriter.create textWriterConf)
      let instance =
        target.initer Fac.emptyRuntime |> run
      let (minionInfo : MinionInfo) =
        { name     = instance.name
          policy   = Supervisor.Delayed (Duration.FromSeconds 1L)
          job      = instance.server
          shutdown = instance.shutdown }

      sup.register *<- minionInfo |> run

      Message.eventVerbose "a supervised message"
      |> Target.logAndWait instance
      sup.shutdown *<-=>- id |> run

    testCase "Supervisor restarts failing procs" <| fun _ ->
      let sup = create internalLogger
      let hasRun = IVar()
      let p sendWill state =
        match state with
        | None ->
          sendWill (box "hello")
          |> Job.map (fun () -> failwith "Arrrgh")
        | Some _ ->
          IVar.tryFill hasRun () |> start
          timeOutMillis 1000 :> Job<_>
      let proc = { name     = PointName.parse "bang"
                   policy   = Supervisor.Delayed (Duration.FromMilliseconds 10L)
                   job      = p
                   shutdown = Ch() }
      sup.register *<- proc |> run
      let result =
        Alt.choose [IVar.read hasRun |> Alt.afterFun (fun () -> true)
                    timeOutMillis 100 |> Alt.afterFun (fun () -> false)]
        |> run
      Expect.isTrue result "Supervisor should restart failing job"

    testCase "Supervisor does not restart exiting procs" <| fun _ ->
      let sup = create internalLogger
      let hasRerun = IVar()
      let rec loop sendWill state =
        match state with
        | None ->
          sendWill (box "hello")
        | Some _ ->
          IVar.tryFill hasRerun () |> start
          timeOutMillis 1000 :> Job<_>
      let proc = { name     = PointName.parse "bang"
                   policy   = Supervisor.Delayed (Duration.FromMilliseconds 10L)
                   job      = loop
                   shutdown = Ch() }
      sup.register *<- proc |> run
      let result =
        Alt.choose [IVar.read hasRerun |> Alt.afterFun (fun () -> true)
                    timeOutMillis 100 |> Alt.afterFun (fun () -> false)]
        |> run
      Expect.isFalse result "Supervisor should not restart exiting jobs"
  ]
