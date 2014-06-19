#if INTERACTIVE
#r "bin/Release/FSharp.Actor.dll"
#r "bin/Release/NodaTime.dll"
#r "bin/Release/Intelliplan.Logary.dll"
#r "bin/Release/Intelliplan.Logary.Riemann.dll"
#endif

open System
open System.Text.RegularExpressions

open FSharp.Actor
open NodaTime

open Logary
open Logary.Configuration

open Logary.Target
open Logary.Logging
open Logary.Targets

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

  /// Curried variant
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
      { new HealthCheck with
          member x.Name = name
          member x.GetValue () =
            try
              counter.NextValue()
              |> float
              |> measureTransform
              |> Metrics.setPath name
              |> HealthChecks.measureToResult
            with
              e -> NoValue
          member x.Dispose () =
            counter.Dispose() }
    | None -> HealthChecks.mkDead name

  let toHealthCheck wpc =
    let inst = wpc.instance |> Option.fold (fun s t -> sprintf ".%s" t) ""
    let name = sprintf "%s.%s%s" wpc.category wpc.counter inst
    toHealthCheckNamed name wpc

open WindowsPerfCounters

let mkMeasure' fValueTr fLevel rawValue =
  let m = Metrics.mkMeasure Gauge (fValueTr rawValue)
  { m with level = fLevel m.value }

module Transforms =
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
    toHealthCheck wpc (Transforms.percentBucket' 100.))

open System.Text

let clr_proc =
  let cat = ".NET CLR Memory"
  let inst = pidToInstance cat (pid ())
  let wpc = { category = cat; counter  = "% Time in GC"; instance = inst }
  let MiB = 1024.f * 1024.f
  let toMiB = (fun v -> v / MiB)
  let tf =
    Transforms.percentBucket 100. [0.05; 0.5] [Info; Warn; Error]
    >> fun measuree ->
      [ "Gen 0 Heap Size", toMiB, "MiB"
        "Gen 1 Heap Size", toMiB, "MiB"
        "Gen 2 Heap Size", toMiB, "MiB"
        "# Gen 0 Collections", id, ""
        "# Gen 1 Collections", id, ""
        "# Gen 2 Collections", id, "" ]
      |> List.map (fun (counter, fval, valUnit) -> mkPc' cat counter inst, fval, valUnit)
      |> List.filter (fun (c, _, _) -> Option.isSome c)
      |> List.fold
          (fun (sb : StringBuilder) (counter, fval, valUnit) ->
            let counter = counter.Value
            let line = String.Format("{0}: {1:0.###} {2}", counter.CounterName, fval(counter.NextValue()), valUnit)
            sb.AppendLine(line) |> ignore
            sb)
          (StringBuilder())
      |> sprintf "%O"
      |> (fun desc -> HealthChecks.setDesc desc measuree)

  toHealthCheck wpc tf

let printAll checks =
  let printSingle (check : HealthCheck) =
    match check.GetValue() with
    | NoValue -> printfn "%s: -" check.Name
    | HasValue v -> printfn ">>> [%O] %s: %f\n%s" v.Measure.level check.Name v.Measure.value v.Description
  checks |> Seq.iter printSingle

// printAll my_appdomain
// printAll [clr_proc]

[<EntryPoint>]
let main argv =
  use logary =
    withLogary "Riemann.Example" (
      withTargets [
        Riemann.create (Riemann.RiemannConf.Create(tags = ["riemann-health"])) "riemann"
        Console.create (Console.ConsoleConf.Default) "console"
      ] >>
      withRules [
        Rule.Create(Regex(@".*"), "riemann", (fun _ -> true), LogLevel.Verbose)
        Rule.Create(Regex(@".*"), "console", (fun _ -> true), LogLevel.Verbose)
      ]
    )

  let logger = logary.GetLogger "Riemann.Example"
  ("disk /", 0.456) ||> Metrics.gauge logger

  0 // return an integer exit code
