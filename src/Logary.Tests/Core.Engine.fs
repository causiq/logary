module Logary.Tests.Engine

open System
open System.Net.NetworkInformation
open Expecto
open Expecto.Flip
open NodaTime
open Hopac
open Hopac.Infixes
open Hopac.Extensions
open Logary
open Logary.Internals
open Logary.Configuration
open Logary.Model

type MockTarget =
  { server: string * (LogaryMessageBase -> LogResult)
    getMessages: unit -> Alt<LogaryMessageBase list> }

let mockTarget name =
  let mailbox = Mailbox<LogaryMessageBase> ()
  let getMsgReq = Ch ()

  let rec loop msgs =
    Alt.choose [
      Mailbox.take mailbox ^=> fun newMsg ->
        loop [ yield! msgs; yield newMsg ]

      getMsgReq ^=> fun reply ->
        reply *<= msgs >>=. loop []
    ]

  let sendMsg x =
    Mailbox.Now.send mailbox x
    LogResult.success

  let getMsgs () = getMsgReq *<-=>- id

  let target =
    { server = name, sendMsg
      getMessages = getMsgs }

  loop [] |> Job.server >>-. target

let run ri pipe (targets: ResizeArray<MockTarget>) =
  let targetsMap =
    targets
      |> Seq.map (fun t -> t.server)
      |> Map.ofSeq

  pipe
    |> Pipe.run ri (fun (msg: LogaryMessageBase) ->
     let allSendJobs =
       msg.targets
       |> Seq.choose (fun targetName -> Map.tryFind targetName targetsMap)
       |> Seq.map (fun target -> target msg)
       |> Job.conCollect

     let allAppendedAcks = IVar ()

     let logToAllTargetAlt = Alt.prepareJob <| fun _ ->
        Job.start (allSendJobs >>= IVar.fill allAppendedAcks)
        >>-. allAppendedAcks

     logToAllTargetAlt ^->. Ok Promise.unit |> HasResult)

  |> Job.map (fun (logMsg, cancellations) ->
     let wrapper x =
       let res = logMsg x |> PipeResult.defaultValue LogResult.success
       res ^=> function
         | Ok ack -> ack :> Job<_>
         | Result.Error _ -> Promise.unit :> Job<_>
     wrapper, cancellations)


let processingComposeJ =
  let processing =
    Events.compose [
      Events.events
        |> Events.minLevel Fatal
        |> Events.setTargets ["1"]

      Events.events
        |> Events.minLevel Warn
        |> Events.setTargets ["2"]
    ]

  job {
    let! ri = emptyRuntime
    let! targets = [mockTarget "1"; mockTarget "2" ] |> Job.seqCollect
    let! sendMsg, ctss = run ri processing targets
    let msgToSend = Model.Event("error message", level=Error)
    do! sendMsg msgToSend

    let! msgsFromEachTarget = targets |> Seq.Con.mapJob (fun t -> t.getMessages())
    let msgs1, msgs2 = msgsFromEachTarget.[0], msgsFromEachTarget.[1]

    msgs1
      |> Expect.isEmpty "Should have no messages in target 1"

    msgs2.Length
      |> Expect.equal "Should have one message in target 2" 1
  }

let processingRoutingJ =
  // context
  let processing =
    Events.compose [
       Events.events
         |> Events.tag "svc1"
         |> Pipe.counter (fun _ -> 1L) (Duration.FromMilliseconds 100.)
         |> Pipe.map (fun counted -> Model.Event(sprintf "counter result is %i within 100 ms" counted, level=Info) :> LogaryMessageBase)
         |> Events.setTarget "1"

       Events.events
         |> Events.whenTagged "gotoTarget2"
         |> Pipe.map (fun msg ->
           let m = Model.Event(msg.getAsOrThrow<Event>())
           m.event <- ":)"
           m :> LogaryMessageBase)
         |> Events.setTarget "2"

       Events.events
        |> Events.minLevel Warn
        |> Events.setTarget "3"

       Events.events
         |> Pipe.bufferTime (Duration.FromMilliseconds 200.)
         |> Pipe.map (fun msgs ->
           let event = sprintf "there are %i msgs every 200 milliseconds" (Seq.length msgs)
           Model.Event(event, level=Info) :> LogaryMessageBase)
         |> Events.setTarget "4"

      //  Events.events
      //  |> Events.tag "metric request latency"
      //  |> Pipe.bufferTime (TimeSpan.FromSeconds 5.)
      //  |> Events.percentile 0.99
      //  |> Pipe.map (fun num -> Message.event Info (sprintf "99th percentile of request latency every rolling 5 second window is %A" num))
    ]

  job {
    // given
    let! targets = [mockTarget "1"; mockTarget "2"; mockTarget "3"; mockTarget "4"] |> Job.seqCollect
    let! ri = emptyRuntime
    let! sendMsg, ctss = run ri processing targets

    let rec generateLog count =
      if count = 100 then
        Alt.always ()
      else
        timeOutMillis 10 ^=> fun _ ->
          let m = Model.GaugeMessage(Gauge.ofMillis 2., Map [ "gauge_name", "db_latency_milliseconds" ])
          m.setContext("service", Value.Str "svc1")
          sendMsg m ^=>. generateLog (count - 1)

    // when
    do! generateLog 100

    do! [1..20] |> Seq.Con.iterJob (fun index -> let m = Event("20 info msgs with tag", level=Info) in m.tag "gotoTarget2"
                                                 sendMsg m)
    do! [1..30] |> Seq.Con.iterJob (fun index -> let m = Event("30 warn msgs", level=Warn)
                                                 sendMsg m)

    // then
    let! targetMessages = targets |> Seq.Con.mapJob (fun t -> t.getMessages())
    let msgs1, msg2, msg3, msg4 =
      targetMessages.[0],targetMessages.[1],targetMessages.[2],targetMessages.[3]

    // target 1
    let allAreFromCounter =
      msgs1 |> List.forall (fun msg ->
        let msg = msg.getAsOrThrow<EventMessage>()
        String.contains "counter result is" msg.event)

    let length = msgs1 |> List.length
    allAreFromCounter
      |> Expect.isTrue "Every message in target 1 comes from the generator/counter."

    (msgs1.Length, 100)
      |> Expect.isLessThan "target 1 should have fewer than 100 messages"


    // target 2
    msg2
      |> List.forall (fun m -> m.hasTag "gotoTarget2" && m.getAsOrThrow<EventMessage>().event = ":)")
      |> Expect.isTrue "all messages should have tag gotoTarget2 and the same event value"

    // target 3
    msg3
      |> List.forall (fun msg -> msg.level >= Warn)
      |> Expect.isTrue "all messages' logging level from target 3 need to be above Warn"

    (msg3.Length, 30)
      |> Expect.isGreaterThan "target 3 should have more than 30 messages"


    // target 4
    msg4
      |> List.forall (fun msg -> String.contains "msgs every 200 milliseconds" (msg.getAsOrThrow<EventMessage>().event))
      |> Expect.isTrue "all msgs from target 4 needs contains the string 'msgs every 200 milliseconds'"

    let length = msg4 |> List.length

    (msg4.Length, 5)
      |> Expect.isGreaterThan "target 4 should have more than 5 messages"

    (msg4.Length, 10)
      |> Expect.isLessThan "target 4 should have fewer than 10 messages"

    // finally
    do! Seq.Con.iterJob Cancellation.cancel ctss
  }

let pingJ =
  // context
  let pingSvc (hostName: string) =
    try
      use p = new Ping()
      let reply = p.Send(hostName,1000)
      if reply.Status <> IPStatus.Success then
        Model.Event(sprintf "ping %s failed, reply status is %s" hostName (reply.Status.ToString ()), level=Error)
          :> LogaryMessageBase
      else
        Model.Event(sprintf "ping %s success" hostName, level=Info)
          :> LogaryMessageBase
    with e ->
      let m = Model.Event(sprintf "ping %s, exception occurred" hostName, level=Fatal)
      m.addExn e
      m :> LogaryMessageBase

  let pingTicker address =
    { new Ticker<_,_, LogaryMessageBase>(pingSvc) with
        override __.reducer state item = state
        override __.handleTick state = state, state address
    }

  let pingFailed = pingTicker "fake.svc"
  let pingOk = pingTicker "github.com"

  let processing =
    Events.compose [
       Events.events
         |> Pipe.tickTimer pingOk (Duration.FromSeconds 1.) // check health every 1 seconds
         |> Pipe.filter (fun msg -> msg.level >= Warn)
         |> Events.setTargets ["TargetForUnhealthySvc"]  // report unhealthy info into some target

       Events.events
         |> Pipe.withTickJob (pingFailed.tickEvery (Duration.FromSeconds 1.))
         |> Pipe.tick pingFailed
         |> Pipe.filter (fun msg -> msg.level >= Warn)
         |> Events.setTargets ["TargetForUnhealthySvc"]
    ]

  job {
    // given
    let! targets = [mockTarget "TargetForUnhealthySvc"] |> Job.seqCollect
    let! ri = emptyRuntime
    let! _, ctss = run ri processing targets

    do! timeOutMillis 3000

    // then
    let! msgsFromEachTarget = targets |> Seq.Con.mapJob (fun t -> t.getMessages ())
    let msgs = Seq.exactlyOne msgsFromEachTarget

    msgs
      |> List.exists (fun msg -> msg.getAsOrThrow<EventMessage>().event.Contains "github")
      |> Expect.isFalse "ping of github"
    msgs
      |> List.exists (fun msg -> msg.getAsOrThrow<EventMessage>().event.Contains "fake.svc")
      |> Expect.isTrue "ping of fake.svc"

    // finally
    do! Seq.Con.iterJob Cancellation.cancel ctss
  }

let conditionalBufferingJ =
  let dt = DateTime.Parse("10:10").ToUniversalTime()
  let timeNow = Instant.FromDateTimeUtc(dt)
  let tenMins = Duration.FromMinutes(10L)

  let keepUntilErrorHappens (msg: LogaryMessageBase, buffered: LogaryMessageBase list) =
    let durationFromFirstBufferedMsg =
      match List.tryHead buffered with
      | Some firstMsg ->
        timeNow - Instant.ofEpoch firstMsg.timestamp
      | None ->
        Duration.MinValue

    if msg.level >= Error then Pipe.BufferAction.Delivery
    elif buffered.Length >= 4 || durationFromFirstBufferedMsg > tenMins then Pipe.BufferAction.Reset
    else Pipe.BufferAction.AddToBuffer

  // context
  let processing =
    Events.compose [
       Events.events
       |> Pipe.bufferConditional keepUntilErrorHappens
       |> Events.flattenSeq
       |> Pipe.map (fun msg ->
          let targets = if msg.level >= Error then Set ["email"] else Set ["dashboard"]
          msg.targets <- msg.targets + targets
          msg)
    ]

  job {
    // given
    let! targets = [mockTarget "email"; mockTarget "dashboard"] |> Job.seqCollect
    let! ri = emptyRuntime
    let! sendMsg, ctss = run ri processing targets

    // when
    let sendMessage level time =
      Model.Event(Guid.NewGuid().ToString("N"),
                         level=level,
                         timestamp=Instant.FromDateTimeUtc(DateTime.Parse(time).ToUniversalTime()).asTimestamp)
      |> sendMsg

    do! sendMessage Info "10:01"
    do! sendMessage Debug "10:02"
    do! sendMessage Verbose "10:05"
    do! sendMessage Info "10:05"
    do! sendMessage Info "10:05"

    let! msgsFromEachTarget = targets |> Seq.Con.mapJob (fun t -> t.getMessages())
    let msgFromEmail, msgFromDashboard = msgsFromEachTarget.[0], msgsFromEachTarget.[1]
    msgFromEmail |> Expect.isEmpty "no error msg occur,so no msg"
    msgFromDashboard |> Expect.isEmpty "no error msg occur,so no msg"

    do! sendMessage Info "10:05"
    do! sendMessage Warn "10:06"
    do! sendMessage Warn "10:07"
    do! sendMessage Error "10:08"
    let! msgs = targets |> Seq.Con.mapJob (fun t -> t.getMessages ())
    let msgFromEmail, msgFromDashboard = msgs.[0], msgs.[1]
    msgFromEmail.Length
      |> Expect.equal "1 error msg send to email" 1
    msgFromDashboard.Length
      |> Expect.equal "3 other msg send to dashboard" 3

    do! sendMessage Fatal "10:09"
    let! msgs = targets |> Seq.Con.mapJob (fun t -> t.getMessages ())
    let msgFromEmail, msgFromDashboard = msgs.[0], msgs.[1]
    msgFromDashboard
      |> Expect.isEmpty "no buffered msg"
    msgFromEmail.Length
      |> Expect.equal "1 error msg continue send to email" 1

    do! sendMessage Info "09:50"
    do! sendMessage Info "09:55"
    do! sendMessage Info "10:05"
    do! sendMessage Info "10:06"
    do! sendMessage Fatal "10:15"
    let! msgs = targets |> Seq.Con.mapJob (fun t -> t.getMessages ())
    let (msgFromEmail, msgFromDashboard) = (msgs.[0],msgs.[1])
    msgFromEmail.Length
      |> Expect.equal "1 Fatal msg send to email" 1
    msgFromDashboard.Length
      |> Expect.equal "2 other msg send to dashboard, fist two are discarded beacuse they are too old" 2

    do! Seq.Con.iterJob Cancellation.cancel ctss
  }

// TODO: model pipe state with MVar from Hopac

[<Tests>]
let tests =
  testList "engine" [
    testList "processing builder" [
      testCaseJob "pipe compose" processingComposeJ
      ptestCaseJob "message routing" processingRoutingJ
    ]

    testList "health checks" [
      // the ping service is just a sample
      ptestCaseJob "ping" pingJ
    ]

    testCaseJob "buffer conditional pipe" conditionalBufferingJ

  ] |> testLabel "logary"