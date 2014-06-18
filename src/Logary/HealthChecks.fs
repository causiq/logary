/// A module that makes it smooth to interact with running/starting/configuration of
/// health checks.
module Logary.HealthChecks

open System
open System.Runtime.CompilerServices

open FSharp.Actor

/// A key in the `data` map inside the `Measure` type.
[<Literal>]
let Description = "description"

/// A key in the `data` map inside the `Measure` type.
[<Literal>]
let Exception = "exception"

/// Sets the description property of the measurement's data map
[<CompiledName "SetDescription">]
let setDesc description m =
  m |> Metrics.setData Description description

/// Sets the exception property of the measurement's data map
[<CompiledName "SetException">]
let setExn e m =
  m |> Metrics.setData Exception e

/// Tries to get the description value from the measure.
[<CompiledName "TryGetDescription">]
let tryGetDesc m =
  m.data |> Map.tryFind Description |> Option.fold (fun s t -> t :?> string) ""

/// Tries to get an exception from the measure
[<CompiledName "TryGetException">]
let tryGetExn m =
  m.data |> Map.tryFind Exception |> Option.map (fun x -> x :?> exn)

/// An implementation of the ResultData interface that wraps a Measure and
/// uses its 'data' Map to read.
type MeasureWrapper(m : Measure) =
  interface ResultData with
    member x.Measure     = m
    member x.Description = tryGetDesc m
    member x.Exception   = tryGetExn m
  override x.ToString() =
    sprintf "HealthCheck(name = %s, exn = %A, value=%f)"
      m.path (tryGetExn m) m.value

/// Transform the metric to a result data.
[<CompiledName "MeasurementToResult">]
let measureToResult (m : Measure) =
  MeasureWrapper m :> ResultData
  |> HasValue

type HealthCheckMessage =
  | GetResult of HealthCheckResult Types.ReplyChannel
  | ShutdownHealthCheck of Acks Types.ReplyChannel

type HealthCheckInstance =
  { actor : HealthCheckMessage IActor }

type private FnCheckerState =
  { last : HealthCheckResult }

let private mkFromFunction fn =
  (fun (inbox : IActor<_>) ->
    let rec running state = async {
      let! msg, mopts = inbox.Receive()
      match msg with
      | GetResult chan ->
        let! x = fn ()
        chan.Reply x
        return! running  { last = x }
      | ShutdownHealthCheck ackChan ->
        ackChan.Reply Ack
        return () }

    running { last = NoValue })

/// Create a new health check from a checking function. This will create an
/// actor but it needs to be registered in the registry for it to work on its
/// own.
let fromFn name f =
  let a = Actor.spawn (Actor.Options.Create(sprintf "logaryRoot/healthCheck/%s" name)) (mkFromFunction f)
  { new HealthCheck with
      member x.Name = name
      member x.LastValue () =
        a
        |> Actor.reqReply GetResult Infinite
        |> Async.RunSynchronously
      member x.Dispose() =
        a
        |> Actor.reqReply ShutdownHealthCheck Infinite |> Async.Ignore
        |> Async.RunSynchronously }
