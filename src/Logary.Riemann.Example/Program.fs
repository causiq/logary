#if INTERACTIVE
#r "bin/Release/FSharp.Actor.dll"
#r "bin/Release/NodaTime.dll"
#r "bin/Release/Intelliplan.Logary.dll"
#r "bin/Release/Intelliplan.Logary.Riemann.dll"
#endif

open System
open System.Diagnostics
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

let pc category counter instance =
  new PerformanceCounter(category, counter, instance, true)

let metricPc name category counter instance measureTransform =
  let counter = pc category counter instance
  { new HealthCheck with
      member x.Name = name
      member x.LastValue () =
        try
          counter.NextValue()
          |> fun v -> printfn "value: %f" v ; v
          |> float
          |> measureTransform name
          |> HealthChecks.setDesc "TODO: process listing"
          |> HealthChecks.measureToResult
        with
          e -> NoValue
      member x.Dispose () =
        counter.Dispose() }

let mkMeasure' fValueTr fLevel path rawValue =
  let m = Metrics.mkMeasure Gauge path (fValueTr rawValue)
  { m with level = fLevel m.value }

/// Finds the bucket that is less than or equal in value to the sample, yielding
/// its corresponding label.
let lteBucket (buckets : _ seq) (labels : _ seq) sample =
  Seq.take (Seq.length buckets) labels // all but last
  |> Seq.map2 (fun b l -> b, l, sample <= b) buckets // find first that is lte
  |> Seq.tryFind (fun (b, l, ok) -> ok) // any matches?
  |> Option.map (fun (_, l, _) -> l) // find its label then
  |> Option.fold (fun s t -> t) (Seq.last labels) // otherwise pick last label

module Transforms =
  let percentBucket divisor =
    mkMeasure'
      (fun (v : float) -> v / divisor)
      (lteBucket [0.8; 0.9] [Info; Warn; Error])

let cpu = metricPc "cpu" "Processor" "% Processor Time" "_Total" (Transforms.percentBucket 100.)

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
