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

  /// Transform the measure to a result data.
  [<CompiledName "AsResult"; Extension>]
  let ofResult (m : _) =
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
    let a = Actor.spawn (Actor.Options.Create(sprintf "logaryRoot/healthCheck/%s" name))
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

  // these replace the HealthChecks' actor implementations:
  module Probe =

    let private exampleProbe _ (* conf, logary conf etc ... *) (inbox : IActor<_>) =
      let rec loop () = async {
        let! msg, _ = inbox.Receive()
        match msg with
        | GetValue (datapoints, replChan) ->
          // what are the values for the requested data points?
          replChan.Reply(datapoints |> List.map (fun dp -> dp, Measure.empty))
          return! loop ()
        | GetDataPoints replChan ->
          // what data points does this probe support?
          replChan.Reply [ DP "min"; DP "mean"; DP "99th percentile" ]
          return! loop ()
        | Update msr ->
          // update the probe with the given measure
          return! loop ()
        | Sample ->
          // read data from external sources and update state
          return! loop ()
        | Terminate ->
          return! shutdown ()
        | Reset ->
          return! loop ()
        }
      and shutdown () = async.Return ()

      loop ()

  // see http://www.mono-project.com/Mono_Performance_Counters
  // see http://www.databasejournal.com/features/mssql/article.php/3932406/Top-10-SQL-Server-Counters-for-Monitoring-SQL-Server-Performance.htm
  // http://www.quest.com/backstage/images/promotions/SQLServer-Perfmonance-Poster.pdf
  // http://www.mssqltips.com/sqlservertip/2460/perfmon-counters-to-identify-sql-server-disk-bottlenecks/
  // http://www.mssqltips.com/sqlservertip/1265/sql-server-database-specific-performance-counters/

  // http://matt.aimonetti.net/posts/2013/06/26/practical-guide-to-graphite-monitoring/
  // https://stackoverflow.com/questions/4455187/wrong-calculated-cpu-usage-using-c-sharp-and-wmi

  /// A module that helps you interact with Windows Performance Counters. Wraps
  /// null-based APIs and guides the programmer with sane names and documentaation.
  module WindowsPerfCounters =
    open System.Diagnostics

    /// A record that encapsulates the known information about a Windows Performance
    /// Counter.
    type WindowsPerfCounter =
      { category : string
        counter  : string
        instance : string option }

    /// The instance that is the sum of all instances. It's the literal `_Total`.
    [<Literal>]
    let AllInstances = "_Total"

    /// Type alias for PerformanceCounterCategory
    type PCC = PerformanceCounterCategory

    /// Create a new performance counter category
    let mkPcc name =
      if PCC.Exists name then PCC name |> Some else None

    /// Checks whether the instance exists in the category
    let instanceExists category instance =
      PCC.InstanceExists(instance, category)

    /// Checks whether the counter in the category exists
    let counterExists category counter =
      PCC.CounterExists(counter, category)

    /// Create a new performance counter given a WindowsPerfCounter record.
    let mkPc { category = cat; counter = cnt; instance = inst } =
      if counterExists cat cnt then
        match inst with
        | Some inst when instanceExists cat inst ->
          new PerformanceCounter(cat, cnt, inst, true) |> Some
        | _ ->
          new PerformanceCounter(cat, cnt, "", true) |> Some
      else
        None

    /// Curried variant of `mkPc` that takes a category, counter and optional
    /// instance and creates an `Option<PerformanceCounter>` from it.
    let mkPc' category counter instance =
      mkPc { category = category; counter = counter; instance = instance }

    /// try to find the instance performance counter for the pid, or return None
    /// if the process e.g. does no longer run and can therefore not be found
    let pidToInstance category pid =
      match mkPcc category with
      | None -> None
      | Some cat ->
        cat.GetInstanceNames()
        |> Array.map (fun instName ->
          match mkPc { category = category; counter = "Process ID"; instance = Some instName } with
          | Some pcProcId when int (pcProcId.NextValue()) = pid ->
            instName
          | _ -> "")
        |> Array.tryFind (fun s -> s.Length > 0)

    /// Gets the current process' id
    let pid () =
      Process.GetCurrentProcess().Id

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
                |> ofResult
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


module internal Play =
  open HealthCheck
  open WindowsPerfCounters

  let mkMeasure' fValueTr fLevel rawValue =
    let m = Measure.mkMeasure "" (fValueTr rawValue)
    { m with m_level = fLevel (getValueFloat m) }

  module Categorisation =
    /// Finds the bucket that is less than or equal in value to the sample, yielding
    /// its corresponding label.
    let lteBucket (buckets : _ seq) (labels : _ seq) sample =
      Seq.take (Seq.length buckets) labels // all but last
      |> Seq.map2 (fun b l -> b, l, sample <= b) buckets // find first that is lte
      |> Seq.tryFind (fun (b, l, ok) -> ok) // any matches?
      |> Option.map (fun (_, l, _) -> l) // find its label then
      |> Option.fold (fun s t -> t) (Seq.last labels) // otherwise pick last label

    /// Divides a given value first by the divisor, then assigns it a bucket of
    /// `levels`
    let percentBucket divisor buckets labels =
      mkMeasure'
        (fun (v : float) -> v / divisor)
        (lteBucket buckets labels)

    /// Divides a given value first by the divisor, then assigns it a bucket of
    /// Info, Warn or Error.
    let percentBucket' divisor =
      percentBucket divisor [0.8; 0.9] [Info; Warn; Error]

  let cpus =
    [ "% Processor Time"
      "% User Time"
      "% Interrupt Time"
      "% Processor Time" ]
    |> List.map (fun counter ->
      let wpc = { category = "Processor"
                  counter  = counter
                  instance = Some AllInstances }
      toHealthCheck wpc (Categorisation.percentBucket' 100.))

  open System.Text

  let clr_proc =
    let cat = ".NET CLR Memory"
    let inst = pidToInstance cat (pid ())
    let wpc = { category = cat; counter  = "% Time in GC"; instance = inst }
    let MiB = 1024.f * 1024.f
    let toMiB = (fun v -> v / MiB)
    let descCounters =
      [ "Gen 0 Heap Size", toMiB, "MiB"
        "Gen 1 Heap Size", toMiB, "MiB"
        "Gen 2 Heap Size", toMiB, "MiB"
        "# Gen 0 Collections", id, ""
        "# Gen 1 Collections", id, ""
        "# Gen 2 Collections", id, "" ]
      |> List.map (fun (counter, fval, valUnit) -> mkPc' cat counter inst, fval, valUnit)
      |> List.filter (fun (c, _, _) -> Option.isSome c)
      |> List.map (fun (c, fval, valUnit) -> c.Value, fval, valUnit)

    let tf =
      Categorisation.percentBucket 100. [0.05; 0.5] [Info; Warn; Error]
      >> fun measuree ->
        descCounters
        |> List.fold
            (fun (sb : StringBuilder) (counter, fval, valUnit) ->
              let line = String.Format("{0}: {1:0.###} {2}", counter.CounterName, fval(counter.NextValue()), valUnit)
              sb.AppendLine(line) |> ignore
              sb)
            (StringBuilder())
        |> sprintf "%O"
        |> (fun desc -> HealthCheck.setDesc desc measuree)

    toHealthCheck wpc tf |> hasResources (descCounters |> List.map (fun (c, _, _) -> c))

  let printAll checks =
    let printSingle (check : healthcheck) =
      match check.GetValue() with
      | NoValue -> printfn "%s: -" check.Name
      | HasValue v ->
        let m = v.Measure
        printfn ">>> [%O] %s: %f\n%s" m.m_level check.Name (getValueFloat m) v.Description
    checks |> Seq.iter printSingle

  // printAll my_appdomain
  // printAll [clr_proc]

  // TODO: register new health checks as conf
  // TODO: register and unregister health checks at runtime
  // TODO: 'acceptor' for metrics
