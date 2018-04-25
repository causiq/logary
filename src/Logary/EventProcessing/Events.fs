namespace Logary.EventProcessing

open Logary
open Logary.Internals
open Hopac
open Hopac.Infixes

[<RequireQualifiedAccessAttribute>]
module Events =
  type Processing = Pipe<Message,Alt<Promise<unit>>,Message> // for logary target based processing

  let events<'r> = Pipe.start<Message,'r>

  let service svc pipe =
    pipe |> Pipe.filter (fun msg -> Message.tryGetContext KnownLiterals.ServiceContextName msg = Some svc)

  let tag tag pipe = pipe |> Pipe.filter (Message.hasTag tag)

  let minLevel level pipe =
    pipe |> Pipe.filter (fun msg -> msg.level >= level)

  /// if msg with no specific sinks, will send to all targets
  let sink (names: string list) pipe =
    pipe |> Pipe.map (Message.addSinks names)

  let flattenToProcessing (pipe: Pipe<seq<Message>,Alt<Promise<unit>>,Message>): Processing =
    pipe
    |> Pipe.chain (fun logWithAck ->
       fun (msgs: seq<_>) ->
          let alllogedAcks = IVar ()

          let logAllConJob =
            msgs
            |> Hopac.Extensions.Seq.Con.mapJob (fun msg ->
               logWithAck msg |> PipeResult.orDefault (Promise.instaPromise))

          let logAllAlt = Alt.prepareJob <| fun _ ->
            Job.start (logAllConJob >>= fun acks -> IVar.fill alllogedAcks acks)
            >>-. alllogedAcks

          logAllAlt ^-> fun acks -> Job.conIgnore acks |> memo
          |> PipeResult.HasResult)

  let compose pipes =
    let allTickTimerJobs = List.collect (fun pipe -> pipe.tickTimerJobs) pipes

    let build =
      fun cont ->
        let allBuildedSource = pipes |> List.map (fun pipe -> pipe.build cont)
        fun sourceItem ->

          let allLoggedAcks = IVar ()

          let logAllConJob =
            allBuildedSource
            |> Hopac.Extensions.Seq.Con.mapJob (fun logWithAck ->
               logWithAck sourceItem |> PipeResult.orDefault Promise.instaPromise)

          let logAllAlt = Alt.prepareJob <| fun _ ->
            Job.start (logAllConJob >>= fun acks -> IVar.fill allLoggedAcks acks)
            >>-. allLoggedAcks

          logAllAlt ^-> fun acks -> Job.conIgnore acks |> memo
          |> PipeResult.HasResult

    { build = build; tickTimerJobs = allTickTimerJobs }