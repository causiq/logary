module Logary.Tests.Engine

open System
open Expecto
open Logary
open Logary.Message
open Hopac
open Hopac.Infixes
open Hopac.Extensions
open Logary.Internals

type MockTarget =
  { server : string * (Message -> Alt<Promise<unit>>)
    getMsgs : unit -> Message list Alt}

let mockTarget name =
  let mailbox = Mailbox<Message> ()
  let getMsgReq = Ch ()

  let rec loop msgs =
    Alt.choose [
      Mailbox.take mailbox ^=> fun newMsg -> loop [yield! msgs; yield newMsg]

      getMsgReq ^=> fun reply ->
        reply *<= msgs >>=. loop msgs
    ]

  let sendMsg x = Mailbox.Now.send mailbox x; Promise.instaPromise
  let getMsgs () = getMsgReq *<-=>- id

  let target =
    { server = (name,sendMsg)
      getMsgs = getMsgs }

  loop [] |> Job.server >>-. target

let simpleProcessing targetName =
    Events.stream
    |> Events.subscribers [
      Events.events |> Events.sink targetName
    ]
    |> Events.toProcessing

let run pipe (targets:ResizeArray<MockTarget>) =
  let targetsMap =
    targets
    |> Seq.map (fun t -> t.server)
    |> List.ofSeq
    |> HashMap.ofList

  pipe
  |> Pipe.run (fun msg ->
     let targetName = Message.tryGetContext "target" msg
     match targetName with
     | Some (targetName) ->
       let target = HashMap.tryFind targetName targetsMap
       match target with
       | Some target ->
         target msg |> HasResult
       | _ -> NoResult
     | _ -> NoResult)
  |> fun runningPipe ->
     runningPipe >>- fun (logMsg, ctss) ->
     let wrapper x =
       let res = logMsg x |> PipeResult.orDefault Promise.instaPromise
       res ^=> fun ack -> ack |> Job.Ignore
     (wrapper, ctss)

open NodaTime
open System.Net.NetworkInformation

let tests =
  [
    testList "processing builder" [

      testCaseAsync "message routing" (job {

        let fiveMinutesEWMATicker = EWMATicker (Duration.FromSeconds 1L, Duration.FromMinutes 5L)

        // context
        let processing =
          Events.stream
          |> Events.subscribers [
             Events.events
             |> Events.service "svc1"
             |> Events.counter (TimeSpan.FromMilliseconds 100.)
             |> Pipe.map (fun counted -> Message.event Info (sprintf "counter result is %i within 100 ms" counted))
             |> Events.sink "1"

             Events.events
             |> Events.tag "gotoTarget2"
             |> Pipe.map (fun msg -> { msg with value = Event ":)"} )
             |> Events.sink "2"

             Events.events |> Events.miniLevel Warn |> Events.sink "3"

             Events.events
             |> Pipe.bufferTime (TimeSpan.FromMilliseconds 200.)
             |> Pipe.map (fun msgs -> Message.event Info (sprintf "there are %i msgs on every 200 milliseconds" (Seq.length msgs)))
             |> Events.sink "4"

            //  Events.events
            //  |> Events.tag "metric request latency"
            //  |> Pipe.bufferTime (TimeSpan.FromSeconds 5.)
            //  |> Events.percentile 0.99
            //  |> Pipe.map (fun num -> Message.event Info (sprintf "99th percentile of request latency every rolling 5 second window is %A" num))

            //  Events.events |> Events.tag "metric request latency"
            //  |> Pipe.choose (fun msg -> msg |> Message.tryGetGauge "some controller/action rpq")
            //  |> Pipe.map (fun (Gauge (v, _)) -> Convert.ToInt64(v))
            //  |> Pipe.withTickJob (fiveMinutesEWMATicker.TickEvery (TimeSpan.FromSeconds 10.))
            //  |> Pipe.tick fiveMinutesEWMATicker
            //  |> Pipe.map (fun rate -> Message.event Info (sprintf "fiveMinutesEWMA of request latency's rate(sample/sec) is %A" rate))
          ]
          |> Events.toProcessing


        // given
        let! targets = [mockTarget "1"; mockTarget "2"; mockTarget "3"; mockTarget "4"] |> Job.seqCollect
        let! (sendMsg, ctss) = run processing targets

        // when
        let rec generateLog count =
          if count = 100 then Alt.always ()
          else
            timeOutMillis 10 ^=> fun _ ->
              (Message.gauge "someGaugeType" 2
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
          let (Event tpl) =  msg.value
          String.contains "counter result is" tpl)
        let length = msgs1 |> List.length
        Expect.isTrue isCounted "each msg should have counter result"
        Expect.isLessThan length 100 "target 1 should have less than 100 msgs"


        // target 2
        let hasTagAndSameEvent = msg2 |> List.forall (fun msg -> Message.hasTag "gotoTarget2" msg && msg.value = Event ":)")
        Expect.isTrue hasTagAndSameEvent "all msgs should have tag gotoTarget2 and same event"

        // target 3
        let isAboveWarn = msg3 |> List.forall (fun msg -> msg.level >= Warn)
        let length = msg3 |> List.length

        Expect.isTrue isAboveWarn "all msgs's logging level from target 3 need above Warn"
        Expect.isGreaterThan length 30 "target 3 should have more than 30 msgs"


        // target 4
        let allMapped = msg4 |> List.forall (fun msg -> msg.value |> fun (Event tpl) -> String.contains "msgs on every 200 milliseconds" tpl)

        let length = msg4 |> List.length

        Expect.isTrue allMapped "all msgs from target 4 needs contains msgs on every 200 milliseconds"
        Expect.isGreaterThan length 5 "target 4 should have more than 5 msgs"
        Expect.isLessThan length 10 "target 4 should have less than 10 msgs"

        // finally
        do! Seq.Con.iterJob Cancellation.cancel ctss
      } |> Job.toAsync)
    ]

    ftestCaseAsync "health checker ping" (job {
      // context
      let pingSvc (hostName : string) =
        printfn "ping...."
        try
          use p = new Ping()
          // very strange case ,when Ping.Send failed, things not working,
          // exception cann't be captured. but throw exception manually can be handled
          // failwith "strange case"
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
        Events.stream
        |> Events.subscribers [
           Events.events
           |> Pipe.withTickJob (pingOk.TickEvery (TimeSpan.FromSeconds 1.)) // check health every 1 seconds
           |> Pipe.tick pingOk
           |> Pipe.filter (fun msg -> msg.level >= Warn)
           |> Events.sink "TargetForUnhealthySvc"  // report unhealthy info into some target

           Events.events
           |> Pipe.withTickJob (pingFailed.TickEvery (TimeSpan.FromSeconds 1.))
           |> Pipe.tick pingFailed
           |> Pipe.filter (fun msg -> msg.level >= Warn)
           |> Events.sink "TargetForUnhealthySvc"
        ]
        |> Events.toProcessing

      // given
      let! targets = [mockTarget "TargetForUnhealthySvc"] |> Job.seqCollect
      let! (_, ctss) = run processing targets
      do! timeOutMillis 3000

      // then
      let! msgsFromEachTarget = targets |> Seq.Con.mapJob (fun t -> t.getMsgs ())
      let msgs = msgsFromEachTarget |> Seq.exactlyOne

      let isGithubSvcDown = msgs |> List.exists (fun msg -> msg.value |> fun (Event tpl) -> String.contains "github" tpl)
      let isFakeSvcDown = msgs |> List.exists (fun msg -> msg.value |> fun (Event tpl) -> String.contains "fake.svc" tpl)
      Expect.isFalse isGithubSvcDown "ping github"
      Expect.isTrue isFakeSvcDown "ping fake.svc"

      // finally
      do! Seq.Con.iterJob Cancellation.cancel ctss
    } |> Job.toAsync)

  ]