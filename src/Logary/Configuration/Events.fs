namespace Logary.Configuration

open Logary
open Logary.Internals
open Hopac
open Hopac.Infixes
open Hopac.Extensions

type Processing = Pipe<Message,LogResult,Message>

[<RequireQualifiedAccessAttribute>]
module Events =

  let events<'r> = Pipe.start<Message, 'r>

  let service svc pipe =
    pipe |> Pipe.filter (fun msg -> Message.tryGetContext KnownLiterals.ServiceContextName msg = Some svc)

  let tag tag pipe =
    pipe |> Pipe.filter (Message.hasTag tag)

  let minLevel level pipe =
    pipe |> Pipe.filter (fun msg -> msg.level >= level)

  /// if msg with no specific sinks, will send to all targets
  let sink (names: string list) pipe =
    pipe |> Pipe.map (Message.addSinks names)

  let flattenSeq (pipe: Pipe<#seq<Message>, LogResult, Message>): Processing =
    pipe
    |> Pipe.chain (fun next ->
      fun (msgs: #seq<_>) ->
        HasResult << Alt.prepareJob <| fun () ->

        msgs
        |> Seq.Con.mapJob (fun msg -> next msg |> PipeResult.orDefault LogResult.success)
        |> Job.map (fun results -> Result.sequence (results.ToArray()))
        |> Job.map (function
          | Ok _ ->
            LogResult.success
          | Result.Error [] ->
            failwithf "No results for %A" (List.ofSeq msgs)
          | Result.Error (e :: _) ->
            Alt.always (Result.Error e))
        )

  let flattenToProcessing (pipe: Pipe<#seq<Message>, LogResult, Message>): Processing =
    pipe
    |> Pipe.chain (fun logWithAck ->
       fun (msgs: seq<_>) ->
          let alllogedAcks = IVar ()

          let logAllConJob =
            msgs
            |> Hopac.Extensions.Seq.Con.mapJob (fun msg ->
               logWithAck msg |> PipeResult.orDefault LogResult.success)

          let logAllAlt = Alt.prepareJob <| fun _ ->
            Job.start (logAllConJob >>= fun acks -> IVar.fill alllogedAcks acks)
            >>-. alllogedAcks

          logAllAlt ^-> fun acks -> Job.conIgnore acks |> memo
          |> PipeResult.HasResult)

  /// build here means dispatch each event/message to all pipes, not chains them.
  let build pipes =
    let allTickTimerJobs = List.collect (fun pipe -> pipe.tickTimerJobs) pipes

    let build =
      fun cont ->
        let allBuildedSource = pipes |> List.map (fun pipe -> pipe.build cont)
        fun sourceItem ->

          let alllogedAcks = IVar ()

          let logAllConJob =
            allBuildedSource
            |> Hopac.Extensions.Seq.Con.mapJob (fun logWithAck ->
               logWithAck sourceItem |> PipeResult.orDefault (Promise.instaPromise))

          let logAllAlt = Alt.prepareJob <| fun _ ->
            Job.start (logAllConJob >>= fun acks -> IVar.fill alllogedAcks acks)
            >>-. alllogedAcks

          logAllAlt ^-> fun acks -> Job.conIgnore acks |> memo
          |> PipeResult.HasResult

    { build = build
      tickTimerJobs = allTickTimerJobs
    }