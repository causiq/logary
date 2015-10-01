namespace Logary

open System

open Logary.DataModel
open Logary.Metric

/// The details a result
type ResultData =
  /// If the health check is for a value (which it probably is) then it should
  /// contain the measure generated.
  abstract Measure     : Message

  /// Gets the description detailing what went badly with the evaluation of the
  /// health check. Useful for drilling down.
  abstract Description : string

  /// Gets the optional exception that was thrown as a part of the evaluation
  /// of the health check.
  //abstract Exception   : exn option

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
  abstract GetValue : unit -> HealthCheckResult

/// A module that makes it smooth to interact with running/starting/configuration of
/// health checks.
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module HealthCheck =

  open System
  open System.Runtime.CompilerServices

  open FSharp.Actor

  open Logary.Internals

  /// A key in the `data` map inside the `Measure` type.
  [<Literal>]
  let Description = "description"

  /// Sets the description property of the measurement's data map
  [<CompiledName "SetDescription">]
  let setDesc (description: string) m =
    Message.field Description description m

  (*
  /// Sets the exception property of the measurement's data map
  [<CompiledName "SetException">]
  let setExn e m =
    Message.addExn e m
  *)

  /// Tries to get the description value from the measure.
  [<CompiledName "TryGetDescription">]
  let tryGetDesc m =
    match Message.tryGetField Description m with
    | (Some (Field (String s, _))) ->  s
    | _ -> ""

  (*
  TODO/BREAKING: Can't be implemented as-is with the new object model

  /// Tries to get an exception from the measure
  [<CompiledName "TryGetException">]
  let tryGetExn m =
    m.m_data |> Map.tryFind Exception |> Option.map (fun x -> x :?> exn)
  *)

  /// An implementation of the ResultData interface that wraps a Measure and
  /// uses its 'data' Map to read.
  type MeasureWrapper(m : Message) =
    interface ResultData with
      member x.Measure     = m
      member x.Description = tryGetDesc m
      //member x.Exception   = tryGetExn m
    override x.ToString() =
      sprintf "HealthCheck(name=%s, value=%A, level=%A)"
        (PointName.joined m.name) m.value m.level

  module Measure =
    /// Transform the measure to a HealthCheck.ResultData.
    let toResult (m : _) =
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
    let a = Actor.spawn (Ns.create (sprintf "hc/%s" name))
                        (mkFromFunction f)
    { new HealthCheck with
        member x.Name = name
        member x.GetValue () =
          a
          |> Actor.reqReply GetResult Infinite
          |> Async.RunSynchronously
        member x.Dispose() =
          a
          |> Actor.reqReply ShutdownHealthCheck Infinite |> Async.Ignore
          |> Async.RunSynchronously }

  /// Create a health check that will never yield a value
  let mkDead name =
    { new HealthCheck with
        member x.GetValue () = NoValue
        member x.Name        = name
        member x.Dispose ()  = () }
