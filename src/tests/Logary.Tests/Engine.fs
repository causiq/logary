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

      getMsgReq ^=> fun reply ->
        //printfn "%A" msgs
        reply *<= msgs >>=. loop msgs
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
open NodaTime
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

    testList "processing builder" [
          
      testCaseAsync "message routing" (job {
        // context
        let processing = 
          Events.stream 
          |> Events.subscribers [
            Pipe.start |> Events.service "svc1" |> Events.counter (TimeSpan.FromMilliseconds 500.) |> Events.sink "1"

            Pipe.start |> Events.tag "gotoTarget2" |> Pipe.map (fun msg -> { msg with value = Event ":)"} ) 
            |> Events.sink "2"

            Pipe.start |> Events.miniLevel Warn |> Events.sink "3"

            // Pipe.start |> Pipe.bufferTime (TimeSpan.FromSeconds 1.) 
            // |> Pipe.map (fun msgs -> Message.event Info (sprintf "there are %i msgs/sec" (Seq.length msgs)))
            // |> Events.sink "4"
          ]
          |> Events.toProcessing

        let! engine = Engine.create processing List.empty
     
        // given
        let! targets = [mockTarget "1"; mockTarget "2"; mockTarget "3"; mockTarget "4"] |> Job.seqCollect
        do! targets |> Seq.Con.iterJob (fun target -> target.server ||> Engine.subscribe engine)
        

        // when 
        let rec generateLog count =
          printfn "%A" System.DateTime.Now
          if count >= 50 then Alt.always ()
          else
            timeOutMillis 100 ^=> fun _ ->
              let msgFac = 
                eventX "100 error msgs with svc" 
                >> Message.setGauge(Int64 2L,Scalar) 
                >> Message.setContext KnownLiterals.ServiceContextName "svc1"
              (Engine.logWithAck engine Error msgFac None ^=> id) ^=>. generateLog (count + 1)
        
        do! generateLog 0
        // do! [1..20] |> Seq.Con.iterJob (fun index -> Engine.logWithAck engine Info (eventX "20 info msgs with tag" >> Message.tag "gotoTarget2") None ^=> id)
        // do! [1..30] |> Seq.Con.iterJob (fun index -> Engine.logWithAck engine Warn (eventX "30 warn msgs") None ^=> id)
        printfn "after generate log"

        // then
        let! msgsFromEachTarget = targets |> Seq.Con.mapJob (fun t -> t.getMsgs ())
        let (msgs1,msg2,msg3,msg4) = (msgsFromEachTarget.[0],msgsFromEachTarget.[1],msgsFromEachTarget.[2],msgsFromEachTarget.[3])

        // target 1
        let isCounted = msgs1 |> List.forall (fun msg -> 
          match msg.value with 
          | Event tpl -> 
          printfn "%s" tpl
          String.contains "counter result is 20" tpl 
          | _ -> false)
        let length = msgs1 |> List.length

        Expect.equal 10 length "target 1 should have 10 msgs"
        Expect.isTrue isCounted "each counted msg should have 20 counter nums"

        // target 2
        let hasTagAndSameEvent = msg2 |> List.forall (fun msg -> Message.hasTag "gotoTarget2" msg && msg.value = Event ":)")
        Expect.isTrue hasTagAndSameEvent "all msgs should have tag gotoTarget2 and same event"

        // target 3
        let isAboveWarn = msg3 |> List.forall (fun msg -> msg.level >= Warn)
        let length = msg3 |> List.length
        
        Expect.isTrue isAboveWarn "all msgs's logging level from target 3 need above Warn"
        Expect.equal 40 length "target 3 should have 40 msgs"


        // target 4
        let allMapped = msg4 |> List.forall (fun msg -> 
          match msg.value with 
          | Event tpl -> String.contains "msgs/sec" tpl 
          | _ -> false)

        let length = msg4 |> List.length

        Expect.isTrue allMapped "all msgs from target 4 needs contains msgs/sec"
        Expect.equal 60 length "target 4 should have 60 msgs"

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


      ptestCaseAsync "shutdown and cancel tick jobs" (job {
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