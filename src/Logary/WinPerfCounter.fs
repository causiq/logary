// see http://www.mono-project.com/Mono_Performance_Counters
// see http://www.databasejournal.com/features/mssql/article.php/3932406/Top-10-SQL-Server-Counters-for-Monitoring-SQL-Server-Performance.htm
// http://www.quest.com/backstage/images/promotions/SQLServer-Perfmonance-Poster.pdf
// http://www.mssqltips.com/sqlservertip/2460/perfmon-counters-to-identify-sql-server-disk-bottlenecks/
// http://www.mssqltips.com/sqlservertip/1265/sql-server-database-specific-performance-counters/

// http://matt.aimonetti.net/posts/2013/06/26/practical-guide-to-graphite-monitoring/
// https://stackoverflow.com/questions/4455187/wrong-calculated-cpu-usage-using-c-sharp-and-wmi

/// A module that helps you interact with Windows Performance Counters. Wraps
/// null-based APIs and guides the programmer with sane names and documentation.
module Logary.WinPerfCounter

open System
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

/// Type alias for System.Diagnostics.PerformanceCounter
type PC = PerformanceCounter

/// Type alias System.Diagnostics.for PerformanceCounterCategory
type PCC = PerformanceCounterCategory

/// Create a new performance counter category
let mkPcc name =
  if PCC.Exists name then PCC name |> Some else None

/// Gets all available performance counter categories
let getPcc () : PCC list =
  PCC.GetCategories() |> Array.toList

/// Gets all available instance names for the given list of performance counter
/// categories
let getInstances (pcc : PCC) : string list =
  pcc.GetInstanceNames() |> Array.toList

/// Gets a list of performance counters for the given instance name and category.
let getCounters (pcc : PCC) (instance : string) : PC list =
  try
    pcc.GetCounters instance |> Array.toList
  with
  | :? InvalidOperationException as e ->
    // instance has gone away, e.g.
    // > System.InvalidOperationException: Instance devenv/61 does not exist in category Thread.
    []

let getAllCounters () =
  getPcc ()
  |> List.map (fun pcc -> pcc, getInstances pcc)
  |> List.map (fun (pcc, instances) ->
    pcc, (instances |> List.map (fun inst -> inst, getCounters pcc inst)))

/// Checks whether the instance exists in the category
let instanceExists category instance =
  PCC.InstanceExists(instance, category)

/// Checks whether the counter in the category exists
let counterExists category counter =
  PCC.CounterExists(counter, category)

/// Gets the next value for the performance counter
let nextValue (pc : PC) =
  pc.NextValue() |> float

/// Create a new performance counter given a WindowsPerfCounter record.
let mkPc { category = cat; counter = cnt; instance = inst } : PC option =
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
      | Some pcProcId when int (nextValue pcProcId) = pid ->
        instName
      | _ -> "")
    |> Array.tryFind (fun s -> s.Length > 0)

/// Gets the current process' id
let pid () =
  Process.GetCurrentProcess().Id

/// Sets the Performance Counter to check metrics that are for all instances
/// running on the server.
let setAllInstances pc =
  { pc with instance = Some AllInstances }

/// Sets the Performance Counter to only check metrics that are sliced to be for
/// the current process.
let setCurrentProcess pc =
  { pc with instance = pidToInstance pc.category (pid ()) }

/// Sets a specific instance on the Performance Counter.
let setInstance (i : string option) pc =
  { pc with instance = i }

module Processor =
  
  /// nice metrics about CPU time
  let cpuTime =
    [ "% Processor Time"
      "% User Time"
      "% Interrupt Time"
      "% Processor Time" ]
    |> List.map (fun counter ->
      { category = "Processor"
        counter  = counter
        instance = Some AllInstances })

module PhysicalDisk =

  let 