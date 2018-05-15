module Logary.Tests.Engine

open System
open Expecto
open FsCheck
open Logary
open Logary.Message
open Hopac
open Hopac.Infixes
open Hopac.Extensions
open Logary.Internals
open Logary.Configuration
open NodaTime
open System.Net.NetworkInformation

type MockTarget =
  { server: string * (Message -> LogResult)
    getMsgs: unit -> Alt<Message list> }

let mockTarget name =
  let mailbox = Mailbox<Message> ()
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
      getMsgs = getMsgs }

  loop [] |> Job.server >>-. target

let run pipe (targets:ResizeArray<MockTarget>) =
  let targetsMap =
    targets
    |> Seq.map (fun t -> t.server)
    |> List.ofSeq
    |> HashMap.ofList

  pipe
  |> Pipe.run (fun msg ->
     let allSendJobs =
       msg
       |> Message.getAllSinks
       |> Seq.choose (fun targetName ->
          HashMap.tryFind targetName targetsMap |> Option.map (fun target -> target msg))
       |> Job.conCollect

     let allAppendedAcks = IVar ()

     let logToAllTargetAlt = Alt.prepareJob <| fun _ ->
        Job.start (allSendJobs >>= IVar.fill allAppendedAcks)
        >>-. allAppendedAcks

     logToAllTargetAlt ^->. Ok Promise.unit |> HasResult)

  |> Job.map (fun (logMsg, ctss) ->
     let wrapper x =
       let res = logMsg x |> PipeResult.orDefault LogResult.success
       res ^=> function
         | Ok ack -> ack :> Job<_>
         | Result.Error _ -> Promise.unit :> Job<_>
     wrapper, ctss)


let tests =
  [
    testList "processing builder" [
      ptestCaseJob "message routing" (job {

        // context
        let processing =
          Events.compose [
             Events.events
             |> Events.service "svc1"
             |> Pipe.counter (fun _ -> 1L) (TimeSpan.FromMilliseconds 100.)
             |> Pipe.map (fun counted -> Message.event Info (sprintf "counter result is %i within 100 ms" counted))
             |> Events.sink ["1"]

             Events.events
             |> Events.tag "gotoTarget2"
             |> Pipe.map (fun msg -> { msg with value = ":)"} )
             |> Events.sink ["2"]

             Events.events |> Events.minLevel Warn |> Events.sink ["3"]

             Events.events
             |> Pipe.bufferTime (TimeSpan.FromMilliseconds 200.)
             |> Pipe.map (fun msgs -> Message.event Info (sprintf "there are %i msgs on every 200 milliseconds" (Seq.length msgs)))
             |> Events.sink ["4"]

            //  Events.events
            //  |> Events.tag "metric request latency"
            //  |> Pipe.bufferTime (TimeSpan.FromSeconds 5.)
            //  |> Events.percentile 0.99
            //  |> Pipe.map (fun num -> Message.event Info (sprintf "99th percentile of request latency every rolling 5 second window is %A" num))
          ]

        // given
        let! targets = [mockTarget "1"; mockTarget "2"; mockTarget "3"; mockTarget "4"] |> Job.seqCollect
        let! sendMsg, ctss = run processing targets

        // when
        let rec generateLog count =
          if count = 100 then Alt.always ()
          else
            timeOutMillis 10 ^=> fun _ ->
              (Message.gaugefs "A" "someGaugeType" 2.
              |> Message.setContext KnownLiterals.ServiceContextName "svc1"
              |> sendMsg)
              ^=>. generateLog (count - 1)

        do! generateLog 100
        do! [1..20] |> Seq.Con.iterJob (fun index -> event Info "20 info msgs with tag" |> Message.tag "gotoTarget2" |> sendMsg)
        do! [1..30] |> Seq.Con.iterJob (fun index -> event Warn "30 warn msgs" |> sendMsg)

        // then
        let! msgsFromEachTarget = targets |> Seq.Con.mapJob (fun t -> t.getMsgs ())
        let (msgs1,msg2,msg3,msg4) = (msgsFromEachTarget.[0],msgsFromEachTarget.[1],msgsFromEachTarget.[2],msgsFromEachTarget.[3])

        // target 1
        let isCounted = msgs1 |> List.forall (fun msg ->
          let tpl =  msg.value
          String.contains "counter result is" tpl)
        let length = msgs1 |> List.length
        Expect.isTrue isCounted "each msg should have counter result"
        Expect.isLessThan length 100 "target 1 should have less than 100 msgs"


        // target 2
        let hasTagAndSameEvent = msg2 |> List.forall (fun msg -> Message.hasTag "gotoTarget2" msg && msg.value = ":)")
        Expect.isTrue hasTagAndSameEvent "all msgs should have tag gotoTarget2 and same event"

        // target 3
        let isAboveWarn = msg3 |> List.forall (fun msg -> msg.level >= Warn)
        let length = msg3 |> List.length

        Expect.isTrue isAboveWarn "all msgs's logging level from target 3 need above Warn"
        Expect.isGreaterThan length 30 "target 3 should have more than 30 msgs"


        // target 4
        let allMapped = msg4 |> List.forall (fun msg -> String.contains "msgs on every 200 milliseconds" msg.value)

        let length = msg4 |> List.length

        Expect.isTrue allMapped "all msgs from target 4 needs contains msgs on every 200 milliseconds"
        Expect.isGreaterThan length 5 "target 4 should have more than 5 msgs"
        Expect.isLessThan length 10 "target 4 should have less than 10 msgs"

        // finally
        do! Seq.Con.iterJob Cancellation.cancel ctss
      })
    ]

    // just for making an health checker example, not very robust for test, so use ptest...
    ptestCaseJob "health checker ping" (job {
      // context
      let pingSvc (hostName: string) =
        try
          use p = new Ping()
          let reply = p.Send(hostName,1000)
          if reply.Status <> IPStatus.Success then
            Message.event Error (sprintf "ping %s failed, reply status is %s" hostName (reply.Status.ToString ()))
          else
            Message.event Info (sprintf "ping %s success" hostName)
        with
        | e ->
          Message.event Fatal (sprintf "ping %s ,occur exception" hostName)
          |> Message.addExn e

      let pingTicker address = {
        new Ticker<_,_,Message> (pingSvc) with
          override this.Folder state item =
            state

          override this.HandleTick state =
            state, state address
      }

      let pingFailed = pingTicker "fake.svc"
      let pingOk = pingTicker "github.com"

      let processing =
        Events.compose [
           Events.events
           |> Pipe.tickTimer pingOk  (TimeSpan.FromSeconds 1.) // check health every 1 seconds
           |> Pipe.filter (fun msg -> msg.level >= Warn)
           |> Events.sink ["TargetForUnhealthySvc"]  // report unhealthy info into some target

           Events.events
           |> Pipe.withTickJob (pingFailed.TickEvery (TimeSpan.FromSeconds 1.))
           |> Pipe.tick pingFailed
           |> Pipe.filter (fun msg -> msg.level >= Warn)
           |> Events.sink ["TargetForUnhealthySvc"]
        ]

      // given
      let! targets = [mockTarget "TargetForUnhealthySvc"] |> Job.seqCollect
      let! (_, ctss) = run processing targets

      do! timeOutMillis 3000

      // then
      let! msgsFromEachTarget = targets |> Seq.Con.mapJob (fun t -> t.getMsgs ())
      let msgs = msgsFromEachTarget |> Seq.exactlyOne

      let isGithubSvcDown = msgs |> List.exists (fun msg -> String.contains "github" msg.value )
      let isFakeSvcDown = msgs |> List.exists (fun msg -> String.contains "fake.svc" msg.value )
      Expect.isFalse isGithubSvcDown "ping github"
      Expect.isTrue isFakeSvcDown "ping fake.svc"

      // finally
      do! Seq.Con.iterJob Cancellation.cancel ctss
    })

    testCaseJob "buffer conditinal pipe" (job {

      let dutiful (msg: Message, buffered: Message list) =
        let dt =DateTime.Parse("10:10").ToUniversalTime()
        let timeNow = Instant.FromDateTimeUtc(dt)
        let tenMins = Duration.FromMinutes(10L)
        let durationFromFirstBufferMsg =
          match buffered |> List.tryHead with
          | Some firstMsg ->
            timeNow - (firstMsg.timestamp |> Instant.ofEpoch)
          | None -> Duration.MinValue
        if msg.level >= Error then Pipe.BufferAction.Delivery
        elif buffered.Length >= 4 || durationFromFirstBufferMsg > tenMins then Pipe.BufferAction.Reset
        else Pipe.BufferAction.AddToBuffer

      // context
      let processing =
        Events.compose [
           Events.events
           |> Pipe.bufferConditional dutiful
           |> Events.flattenSeq
           |> Pipe.map (fun msg ->
              let sinks = if msg.level >= Error then ["email"] else ["dashboard"]
              msg |> Message.addSinks sinks)
        ]

      // given
      let! targets = [mockTarget "email"; mockTarget "dashboard"] |> Job.seqCollect
      let! sendMsg, ctss = run processing targets

      // when
      let makeMsg level time =
        Message.event level (Guid.NewGuid().ToString("N"))
        |> Message.setTimestamp (Instant.FromDateTimeUtc(DateTime.Parse(time).ToUniversalTime()))
        |> sendMsg

      do! makeMsg Info "10:01"
      do! makeMsg Debug "10:02"
      do! makeMsg Verbose "10:05"
      do! makeMsg Info "10:05"
      do! makeMsg Info "10:05"

      let! msgsFromEachTarget = targets |> Seq.Con.mapJob (fun t -> t.getMsgs ())
      let (msgFromEmail, msgFromDashboard) = (msgsFromEachTarget.[0],msgsFromEachTarget.[1])
      Expect.isEmpty msgFromEmail "no error msg occur,so no msg"
      Expect.isEmpty msgFromDashboard "no error msg occur,so no msg"

      do! makeMsg Info "10:05"
      do! makeMsg Warn "10:06"
      do! makeMsg Warn "10:07"
      do! makeMsg Error "10:08"
      let! msgsFromEachTarget = targets |> Seq.Con.mapJob (fun t -> t.getMsgs ())
      let (msgFromEmail, msgFromDashboard) = (msgsFromEachTarget.[0],msgsFromEachTarget.[1])
      Expect.equal msgFromEmail.Length 1 "1 error msg send to email"
      Expect.equal msgFromDashboard.Length 3 "3 other msg send to dashboard"

      do! makeMsg Fatal "10:09"
      let! msgsFromEachTarget = targets |> Seq.Con.mapJob (fun t -> t.getMsgs ())
      let (msgFromEmail, msgFromDashboard) = (msgsFromEachTarget.[0],msgsFromEachTarget.[1])
      Expect.isEmpty msgFromDashboard "no bufferd msg"
      Expect.equal msgFromEmail.Length 1 "1 error msg continue send to email"

      do! makeMsg Info "09:50"
      do! makeMsg Info "09:55"
      do! makeMsg Info "10:05"
      do! makeMsg Info "10:06"
      do! makeMsg Fatal "10:15"
      let! msgsFromEachTarget = targets |> Seq.Con.mapJob (fun t -> t.getMsgs ())
      let (msgFromEmail, msgFromDashboard) = (msgsFromEachTarget.[0],msgsFromEachTarget.[1])
      Expect.equal msgFromEmail.Length 1 "1 Fatal msg send to email"
      Expect.equal msgFromDashboard.Length 2 "2 other msg send to dashboard, fist two are discarded beacuse they are too old"

      do! Seq.Con.iterJob Cancellation.cancel ctss

    })
  ]