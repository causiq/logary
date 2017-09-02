module Logary.Tests.Engine

open System
open Expecto
open Logary
open Logary.Message
open Hopac
open Hopac.Infixes
open Hopac.Extensions

type MockTarget = 
  { server : string * (Message -> unit Job)
    getMsgs : unit -> Message list Alt}

let mockTarget name =
  let mailbox = Mailbox<Message> ()
  let getMsgReq = Ch ()

  let rec loop msgs = 
    Alt.choose [
      Mailbox.take mailbox ^=> fun newMsg -> loop [yield! msgs; yield newMsg]

      getMsgReq ^=> fun reply -> reply *<= msgs >>=. loop msgs
    ]

  let sendMsg = Mailbox.send mailbox
  let getMsgs () = getMsgReq *<-=>- id

  let target = 
    { server = (name,sendMsg)
      getMsgs = getMsgs }
  
  loop [] |> Job.start >>-. target

let simpleProcessing targetName = 
    Events.stream 
    |> Events.subscribers [
      Pipe.start |> Events.sink targetName
    ]
    |> Events.toProcessing

let tests =
  [
    testCaseAsync "play" (job {
      let processing =
        Events.stream
        |> Events.subscribers [
          Pipe.start
          |> Events.service "svc1"
          |> Events.sink "console"

          Pipe.start
          |> Events.service "svc2"
          |> Events.sink "test"
        ]
        |> Events.toProcessing

      let! engine = Engine.create processing List.empty

      // given
      let mref = IVar ()
      let sink message = IVar.fill mref message
      do! Engine.subscribe engine "test" sink

      let eventSvc1 = ( eventX "Hello World" ) >> Message.setContext KnownLiterals.ServiceContextName "svc1"
      let eventSvc2 = ( eventX "another hello world" ) >> Message.setContext KnownLiterals.ServiceContextName "svc2"

      let! isDone = Engine.logWithAck engine Verbose eventSvc1 None
      do! isDone

      Expect.isFalse mref.Full "Should not have value"

      let! isDone = Engine.logWithAck engine Verbose eventSvc2 None
      do! isDone

      Expect.isTrue mref.Full "Should have value"
      let! res = IVar.read mref
      Expect.equal res.value (Event "another hello world") "Should have event"

      // finally
      do! Engine.shutdown engine
      
    } |> Job.toAsync)

    ptestList "processing builder" [
          
      testCaseAsync "message routing" (job {
        
        // context
        let processing = 
          Events.stream 
          |> Events.subscribers [
            Pipe.start |> Events.service "svc1" |> Events.counter (TimeSpan.FromSeconds 3.) |> Events.sink "1"

            Pipe.start |> Events.tag "gotoTarget2" 
            |> Pipe.map (fun msg -> { msg with value = Event "all msg with tag gotoTarget2 should map to same value"} ) 
            |> Events.sink "2"

            Pipe.start |> Pipe.filter ( fun msg -> msg.level = Fatal) |> Events.sink "3"

            Pipe.start |> Pipe.bufferTime (TimeSpan.FromSeconds 1.) 
            |> Pipe.map (fun msgs -> Message.event Info (sprintf "there are %i msgs/sec" (Seq.length msgs)))
            |> Events.sink "4"
          ]
          |> Events.toProcessing

        let! engine = Engine.create processing List.empty
     
        // given
        let! targets = [mockTarget "1"; mockTarget "2"; mockTarget "3"; mockTarget "4"] |> Job.seqCollect
        do! targets |> Seq.Con.iterJob (fun target -> target.server ||> Engine.subscribe engine)
        

        // when 
        // generate log msg
        failtest "tobe done"


        // then
        let! msgsFromEachTarget = targets |> Seq.Con.mapJob (fun t -> t.getMsgs ())
        let (msgs1,msg2,msg3,msg4) = (msgsFromEachTarget.[0],msgsFromEachTarget.[1],msgsFromEachTarget.[2],msgsFromEachTarget.[3])



        // finally
        do! Engine.shutdown engine
      } |> Job.toAsync)


    ]

    testCaseAsync "engine middleware" (job {
        let targetName = "a"
        let processing = simpleProcessing targetName
        let engineMids = [Middleware.host "host1"; Middleware.service "svc1"]

        let! engine = Engine.create processing engineMids
        let! target = mockTarget targetName
        do! target.server ||> Engine.subscribe engine

        // when
        let tpl = "Hello World"
        let msgFac = (eventX tpl)
        let userCtxName = "loggerMid"
        let loggerMid = Middleware.context userCtxName "from logger"
        let! ack = Engine.logWithAck engine Info msgFac (Some loggerMid)
        do! ack

        // then 
        let! msgs = target.getMsgs ()
        let msg = List.exactlyOne msgs
        Expect.isSome (Message.tryGetContext KnownLiterals.HostContextName msg) "should have host"
        Expect.isSome (Message.tryGetContext KnownLiterals.ServiceContextName msg) "should have svs"
        Expect.isSome (Message.tryGetContext userCtxName msg) "should have user ctx vale"
        Expect.isNone (Message.tryGetContext "xxx" msg) "should have not undefined ctx vale"

        // finally
        do! Engine.shutdown engine
    } |> Job.toAsync)

    testList "lifetime" [
      testCaseAsync "create and shutdown" (job {
        
        let targetName = "a"
        let processing = simpleProcessing targetName

        let! engine = Engine.create processing List.empty
        let! target = mockTarget targetName
        do! target.server ||> Engine.subscribe engine

        // when
        let tpl = "Hello World"
        let msgFac = (eventX tpl)
        let! ack = Engine.logWithAck engine Info msgFac None
        do! ack
        let! ack = Engine.logWithAck engine Verbose msgFac None
        do! ack

        // then 
        let! msgs = target.getMsgs ()
        let exist event = msgs |> List.exists (fun msg -> msg.level = event.level && msg.value = event.value)
        Expect.isTrue (exist (eventX tpl Info)) "Should have info event when engine is running"
        Expect.isTrue (exist (eventX tpl Verbose)) "Should have verbose event when engine is running"

        do! Engine.shutdown engine
        Expect.equal 2 (List.length msgs) "Should have just 2 events after engine shutdown"

      } |> Job.toAsync)


      testCaseAsync "shutdown and cancel tick jobs" (job {
        failtest "tobe done"
        let targetName = "a"
        let processing = simpleProcessing targetName

        let! engine = Engine.create processing List.empty
        let! target = mockTarget targetName
        do! target.server ||> Engine.subscribe engine

        // then 
        let! msgs = target.getMsgs ()

        do! Engine.shutdown engine
        Expect.equal 0 (List.length msgs) "Tick Job Should Stop"

      } |> Job.toAsync)
    ]

  ]