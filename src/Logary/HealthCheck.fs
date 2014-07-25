namespace Logary

open System

open Logary.Measure
open Logary.Metric

/// The details a result
type ResultData =
  /// If the health check is for a value (which it probably is) then it should
  /// contain the measure generated.
  abstract Measure     : ``measure``

  /// Gets the description detailing what went badly with the evaluation of the
  /// health check. Useful for drilling down.
  abstract Description : string

  /// Gets the optional exception that was thrown as a part of the evaluation
  /// of the health check.
  abstract Exception   : exn option

/// A result of the health check. Either Healthy or Unhealthy
type HealthCheckResult =
  /// This health check has no value available.
  | NoValue
  /// The health check has a value.
  | HasValue of ResultData

/// You can centralise the service's health checks by registering instances
/// of this interface.
type healthcheck =
  inherit Named
  inherit IDisposable
  /// Performs a check with the health check.
  abstract GetValue : unit -> HealthCheckResult

/// A module that makes it smooth to interact with running/starting/configuration of
/// health checks.
module HealthCheck =

  open System
  open System.Runtime.CompilerServices

  open FSharp.Actor

  open Logary.Internals
  open Logary.Measure

  /// A key in the `data` map inside the `Measure` type.
  [<Literal>]
  let Description = "description"

  /// A key in the `data` map inside the `Measure` type.
  [<Literal>]
  let Exception = "exception"

  /// Sets the description property of the measurement's data map
  [<CompiledName "SetDescription">]
  let setDesc description m =
    m |> setData Description description

  /// Sets the exception property of the measurement's data map
  [<CompiledName "SetException">]
  let setExn e m =
    m |> setData Exception e

  /// Tries to get the description value from the measure.
  [<CompiledName "TryGetDescription">]
  let tryGetDesc m =
    m.m_data |> Map.tryFind Description |> Option.fold (fun s t -> t :?> string) ""

  /// Tries to get an exception from the measure
  [<CompiledName "TryGetException">]
  let tryGetExn m =
    m.m_data |> Map.tryFind Exception |> Option.map (fun x -> x :?> exn)

  /// An implementation of the ResultData interface that wraps a Measure and
  /// uses its 'data' Map to read.
  type MeasureWrapper(m : _) =
    interface ResultData with
      member x.Measure     = m
      member x.Description = tryGetDesc m
      member x.Exception   = tryGetExn m
    override x.ToString() =
      sprintf "HealthCheck(name=%s, exn=%A, value=%A, level=%A)"
        m.m_path (tryGetExn m) m.m_value m.m_level

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
    { new healthcheck with
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
    { new healthcheck with
        member x.GetValue () = NoValue
        member x.Name        = name
        member x.Dispose ()  = () }
      
  module WinPerfCounter =
    open Logary.WinPerfCounter
    /// Create a new HealthCheck from a WindowsPerfCounter record and a transformation
    /// function `measureTransform`.
    ///
    /// String.empty means no value as e.g. `instance`. Also takes a
    /// `measureTransform` function that allows the caller to customize the value
    /// in a Measure before returning. Suggested is to use `HealthChecks.setDesc`
    /// to give the measure a nice description with detailed data.
    let toHealthCheckNamed name wpc measureTransform =
      match mkPc wpc with
      | Some counter ->
        { new healthcheck with
            member x.Name = name
            member x.GetValue () =
              try
                counter.NextValue()
                |> float
                |> measureTransform
                |> Measure.setPath name
                |> Measure.toResult
              with
                e -> NoValue
            member x.Dispose () =
              counter.Dispose() }
      | None -> mkDead name

    let toHealthCheck wpc =
      let inst = wpc.instance |> Option.fold (fun s t -> sprintf ".%s" t) ""
      let name = sprintf "%s.%s%s" wpc.category wpc.counter inst
      toHealthCheckNamed name wpc

    /// Takes a list of IDisposable things (performance counters, perhaps?) and
    /// wraps the call to Dispose() of the inner health check with calls to
    /// Dispose for each of the resources
    let hasResources (disposables : #IDisposable seq) (hc : healthcheck) =
      { new healthcheck with
          member x.Name = hc.Name
          member x.GetValue() = hc.GetValue()
          member x.Dispose() =
            disposables |> Seq.iter (fun d -> d.Dispose())
            hc.Dispose() }

 
