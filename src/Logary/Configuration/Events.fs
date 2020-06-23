namespace Logary.Configuration

open Logary
open Logary.Internals
open Hopac
open Hopac.Infixes
open Hopac.Extensions

type Processing = Pipe<Model.LogaryMessageBase, LogResult, Model.LogaryMessageBase>

[<RequireQualifiedAccess>]
module Events =
  let events<'r> = Pipe.start<Model.LogaryMessageBase, 'r>

  let hasField tag pipe: Processing =
    pipe
      |> Pipe.filter (fun m -> m.fields.ContainsKey tag)

  let minLevel level pipe: Processing =
    pipe
      |> Pipe.filter (fun msg -> msg.level >= level)

  /// If the message declares no specific sinks, it will be sent to all targets.
  let setTargets (names: #seq<string>) pipe: Processing =
    pipe
      |> Pipe.map (fun (m: Model.LogaryMessageBase) ->
        m.targets <- Set.ofSeq names
        m)

  let tag (tag: string) pipe: Processing =
    pipe
      |>  Pipe.map (fun (m: Model.LogaryMessageBase) ->
        m.tag tag
        m)

  let whenTagged (tag: string) pipe: Processing =
    pipe
      |> Pipe.filter (fun m -> m.hasTag tag)

  let untag tag pipe: Processing =
    pipe
      |> Pipe.map (fun (m: Model.LogaryMessageBase) ->
        m.untag tag
        m)

  let setTarget name pipe: Processing =
    pipe
      |> setTargets [ name ]

  let flattenSeq (pipe: Pipe<#seq<Model.LogaryMessageBase>, LogResult, Model.LogaryMessageBase>): Processing =
    pipe
      |> Pipe.chain (fun next ->
          fun (msgs: #seq<_>) ->
            HasResult << Alt.prepareJob <| fun () ->

            let putAllPromises = IVar ()

            msgs |> Seq.Con.mapJob (fun msg -> next msg |> PipeResult.defaultValue LogResult.success)
            >>= IVar.fill putAllPromises
            |> Job.start
            >>-. putAllPromises ^-> ProcessResult.reduce)

  /// Compose here means dispatch each event/message to all pipes, not chain all pipes together.
  let compose pipes =
    let allTickTimerJobs =
      pipes |> List.collect (fun pipe -> pipe.tickTimerJobs)

    let build =
      fun cont ->
        let composedSource = pipes |> List.map (fun pipe -> pipe.build cont)

        fun sourceItem ->
          HasResult << Alt.prepareJob <| fun () ->

          let allLoggedAcks = IVar ()

          composedSource |> Hopac.Extensions.Seq.Con.mapJob (fun logWithAck ->
             logWithAck sourceItem |> PipeResult.defaultValue (LogResult.success))
          >>= IVar.fill allLoggedAcks
          |> Job.start
          >>-. allLoggedAcks ^-> ProcessResult.reduce

    { build = build
      tickTimerJobs = allTickTimerJobs
    }