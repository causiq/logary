namespace Logary

open System
open Logary
open Logary.Metric
open Hopac

/// The details a result
type ResultData =
  /// If the health check is for a value (which it probably is) then it should
  /// contain the measure generated.
  abstract message     : Message

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
  abstract getValue : unit -> Job<HealthCheckResult>

/// A module that makes it smooth to interact with running/starting/configuration of
/// health checks.
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module HealthCheck =

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
    | (Some (Field (String s, _))) -> Some s
    | _ -> None

  /// An implementation of the ResultData interface that wraps a Measure and
  /// uses its 'data' Map to read.
  type MessageWrapper(m : Message) =
    interface ResultData with
      member x.message     = m
      member x.description = defaultArg (tryGetDesc m) ""
    override x.ToString() =
      sprintf "HealthCheck(name=%s, value=%A, level=%A)"
        (PointName.joined m.name) m.value m.level

  module HealthCheckResult =
    /// Transform the measure to a HealthCheck.ResultData.
    let ofMessage (m : _) =
      MessageWrapper m :> ResultData
      |> HasValue

  module Message =
    let toHealthCheckResult (m : _) =
      MessageWrapper m :> ResultData
      |> HasValue

  type HealthCheckMessage =
    | GetResult of IVar<HealthCheckResult>
    | ShutdownHealthCheck of IVar<Acks>

  type HealthCheckInstance =
    { reqCh : HealthCheckMessage Ch }

  type private FnCheckerState =
    { last : HealthCheckResult }

  let private mkFromFunction (fn : unit -> Job<HealthCheckResult>) =
    let ch = { reqCh = Ch.Now.create () }

    let rec running _ = job {
        let! req = Ch.take ch.reqCh
        match req with
        | GetResult ivar ->
          let! x = fn ()
          do! IVar.fill ivar x
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
            let! ivar = IVar.create ()
            do! Ch.send ch.reqCh (GetResult ivar)
            return! ivar
          })

        member x.Dispose() =
          // TODO: consider non concurrent disposing method
          (job {
            let! ack = IVar.create ()
            do! Ch.give ch.reqCh (ShutdownHealthCheck ack)
            do! Alt.Ignore <| IVar.read ack
          }) |> Job.Global.run
    }

  /// Create a health check that will never yield a value
  let createDead name =
    { new HealthCheck with
        member x.getValue () = Job.result NoValue
        member x.name        = name
        member x.Dispose ()  = () }
