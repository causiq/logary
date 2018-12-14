namespace Logary.Configuration

open Logary
open Logary.Internals
open Hopac
open Hopac.Infixes
open Hopac.Extensions

type Processing = Pipe<Message, LogResult, Message>

[<RequireQualifiedAccessAttribute>]
module Events =
  let events<'r> = Pipe.start<Message, 'r>

  let service svc pipe =
    pipe |> Pipe.filter (fun msg -> Message.tryGetContext KnownLiterals.ServiceContextName msg = Some svc)

  let tag tag pipe =
    pipe |> Pipe.filter (Message.hasTag tag)

  let minLevel level pipe =
    pipe |> Pipe.filter (fun msg -> msg.level >= level)

  /// if msg with no specific sinks, it will send to all targets
  let sink (names: string list) pipe =
    pipe |> Pipe.map (Message.addSinks names)

  let flattenSeq (pipe: Pipe<#seq<Message>, LogResult, Message>): Processing =
    pipe
    |> Pipe.chain (fun next ->
      fun (msgs: #seq<_>) ->
        HasResult << Alt.prepareJob <| fun () ->

        let putAllPromises = IVar ()

        msgs
        |> Seq.Con.mapJob (fun msg -> next msg |> PipeResult.orDefault LogResult.success)
        >>= IVar.fill putAllPromises
        |> Job.start
        >>-. putAllPromises ^-> ProcessResult.reduce
    )

  /// build here means dispatch each event/message to all pipes, not chains them.
  let build pipes =
    let allTickTimerJobs = List.collect (fun pipe -> pipe.tickTimerJobs) pipes

    let build =
      fun cont ->
        let allBuildedSource = pipes |> List.map (fun pipe -> pipe.build cont)
        fun sourceItem ->
          HasResult << Alt.prepareJob <| fun () ->
            let alllogedAcks = IVar ()

            allBuildedSource
            |> Hopac.Extensions.Seq.Con.mapJob (fun logWithAck ->
               logWithAck sourceItem |> PipeResult.orDefault (LogResult.success))
            >>= IVar.fill alllogedAcks
            |> Job.start
            >>-. alllogedAcks ^-> ProcessResult.reduce

    { build = build
      tickTimerJobs = allTickTimerJobs
    }