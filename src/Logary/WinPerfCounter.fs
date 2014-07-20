// see http://www.mono-project.com/Mono_Performance_Counters
// see http://www.databasejournal.com/features/mssql/article.php/3932406/Top-10-SQL-Server-Counters-for-Monitoring-SQL-Server-Performance.htm
// http://www.quest.com/backstage/images/promotions/SQLServer-Perfmonance-Poster.pdf
// http://www.mssqltips.com/sqlservertip/2460/perfmon-counters-to-identify-sql-server-disk-bottlenecks/
// http://www.mssqltips.com/sqlservertip/1265/sql-server-database-specific-performance-counters/

// http://matt.aimonetti.net/posts/2013/06/26/practical-guide-to-graphite-monitoring/
// https://stackoverflow.com/questions/4455187/wrong-calculated-cpu-usage-using-c-sharp-and-wmi

/// A module that helps you interact with Windows Performance Counters. Wraps
/// null-based APIs and guides the programmer with sane names and documentaation.
module Logary.WinPerfCounter

open System.Diagnostics

/// A record that encapsulates the known information about a Windows Performance
/// Counter.
type PerfCounter =
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
