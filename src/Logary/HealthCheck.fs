namespace Logary

open System

open Logary
open Logary.Metric

/// The details a result
type ResultData =
  /// If the health check is for a value (which it probably is) then it should
  /// contain the measure generated.
  abstract msr         : Message

  /// Gets the description detailing what went badly with the evaluation of the
  /// health check. Useful for drilling down.
  abstract description : string

/// A result of the health check. Either Healthy or Unhealthy
type HealthCheckResult =
  /// This health check has no value available.
  | NoValue
  /// The health check has a value.
  | HasValue of ResultData

/// You can centralise the service's health checks by registering instances
/// of this interface.
type HealthCheck =
  inherit Named
  inherit IDisposable
  /// Performs a check with the health check.
  abstract getValue : unit -> HealthCheckResult

/// A module that makes it smooth to interact with running/starting/configuration of
/// health checks.
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module HealthCheck =

  open System
  open System.Runtime.CompilerServices

  open Hopac

  open Logary.Internals

  /// A key in the `data` map inside the `Measure` type.
  [<Literal>]
  let Description = "description"

  /// Sets the description property of the measurement's data map
  [<CompiledName "SetDescription">]
  let setDesc (description: string) m =
    Message.field Description description m

  /// Tries to get the description value from the measure.
  [<CompiledName "TryGetDescription">]
  let tryGetDesc m =
    match Message.tryGetField Description m with
    | (Some (Field (String s, _))) ->  s
    | _ -> ""

  /// An implementation of the ResultData interface that wraps a Measure and
  /// uses its 'data' Map to read.
  type MeasureWrapper(m : Message) =
    interface ResultData with
      member x.msr         = m
      member x.description = tryGetDesc m
      //member x.Exception   = tryGetExn m
    override x.ToString() =
      sprintf "HealthCheck(name=%s, value=%A, level=%A)"
        (PointName.joined m.name) m.value m.level

  module Message =
    /// Transform the measure to a HealthCheck.ResultData.
    let toResult (m : _) =
      MeasureWrapper m :> ResultData
      |> HasValue

  type HealthCheckMessage =
    | GetResult of Ch<HealthCheckResult>
    | ShutdownHealthCheck of IVar<Acks>

  type HealthCheckInstance =
    { reqCh      : HealthCheckMessage Ch }

  type private FnCheckerState =
    { last : HealthCheckResult }

  let private mkFromFunction (fn : unit -> Job<HealthCheckResult>) =
    let ch = { reqCh = Ch.Now.create () }

    let rec running _ = job {
        let! req = Ch.take ch.reqCh
        match req with
        | GetResult resCh ->
          let! x = fn ()
          do! Ch.give resCh x
          return! running { last = x }
        | ShutdownHealthCheck ivar ->
          do! IVar.fill ivar Ack
          return ()
      }
    (ch, running {last = NoValue })

  let fromFn name f =
    let (ch, fjob) = mkFromFunction f
    Job.start fjob |> ignore

    { new HealthCheck with
        member x.name = name
        member x.getValue () =
          (job {
            // TODO / PERF?: This channel could be reused or replaced with an IVar
            let! responseCh = Ch.create ()
            do! Ch.give ch.reqCh (GetResult (responseCh))
            return! Ch.take responseCh
          }) |> Job.Global.run
        member x.Dispose() =
          (job {
            let! ack = IVar.create ()
            do! Ch.give ch.reqCh (ShutdownHealthCheck ack)
            do! Alt.Ignore <| IVar.read ack
          }) |> Job.Global.run
    }

  /// Create a health check that will never yield a value
  let createDead name =
    { new HealthCheck with
        member x.getValue () = NoValue
        member x.name        = name
        member x.Dispose ()  = () }
