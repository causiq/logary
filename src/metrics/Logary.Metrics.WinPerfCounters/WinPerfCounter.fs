// see http://www.mono-project.com/Mono_Performance_Counters
// see http://www.databasejournal.com/features/mssql/article.php/3932406/Top-10-SQL-Server-Counters-for-Monitoring-SQL-Server-Performance.htm
// http://www.quest.com/backstage/images/promotions/SQLServer-Perfmonance-Poster.pdf
// http://www.mssqltips.com/sqlservertip/2460/perfmon-counters-to-identify-sql-server-disk-bottlenecks/
// http://www.mssqltips.com/sqlservertip/1265/sql-server-database-specific-performance-counters/

// http://matt.aimonetti.net/posts/2013/06/26/practical-guide-to-graphite-monitoring/
// https://stackoverflow.com/questions/4455187/wrong-calculated-cpu-usage-using-c-sharp-and-wmi

/// A module that helps you interact with Windows Performance Counters. Wraps
/// null-based APIs and guides the programmer with sane names and documentation.
module Logary.Metrics.WinPerfCounter

open System
open System.Diagnostics
open Logary

/// Type alias System.Diagnostics.for PerformanceCounterCategory
type PCC = PerformanceCounterCategory

/// Type alias for System.Diagnostics.PerformanceCounterCategoryType
type PCCT = PerformanceCounterCategoryType

/// Type alias for System.Diagnostics.PerformanceCounter
type PC = PerformanceCounter

/// There are performance counters in these configurations, always with a category:
///
///   * No instance, just a counter
///   * _Total (aka AllInstances in Logary)
///   * Specific instance, which may or may not exist for long
///
/// A record that encapsulates the known information about a Windows Performance
/// Counter.
///
/// See http://msdn.microsoft.com/en-us/library/vstudio/fxk122b4%28v=vs.100%29.aspx
///
/// Updated every 400 ms by Windows.
type PerfCounter =
  { category : string
    counter  : string
    instance : Instance }  

and Instance =
  /// This PerfCounter has a very specific instance assigned
  | Instance of string
  /// This PerfCounter does not have any instances
  | NotApplicable

module KnownInstances =
  /// The instance that is the sum of all instances. It's the literal `_Total`.
  [<Literal>]
  let _Total = "_Total"

  /// The instance that runs on operating system level.
  [<Literal>]
  let _Global_ = "_Global_"

  [<Literal>]
  let Default = "Default"

/// Create a new performance counter category. It may not exist on the system
/// that you are running the code on; hence the option-based return value
let getPCC name =
  if PCC.Exists name then PCC name |> Some else None

/// Gets all available performance counter categories
let getAllPCC () : PCC list =
  PCC.GetCategories() |> Array.toList

/// Gets all available instance names for the given list of performance counter
/// categories
let getInstances (pcc : PCC) : _ list =
  pcc.GetInstanceNames() |> Array.toList |> List.map Instance

/// Checks whether the instance exists in the category
let instanceExists category instance =
  PCC.InstanceExists(instance, category)

(*
Reason for try-with:

System.InvalidOperationException: Category does not exist.
   at System.Diagnostics.PerformanceCounterLib.CounterExists(String machine, String category, String counter)
   at System.Diagnostics.PerformanceCounterCategory.CounterExists(String counterName, String categoryName, String machineName)
   at System.Diagnostics.PerformanceCounterCategory.CounterExists(String counterName, String categoryName)
   at Logary.Metrics.WinPerfCounter.counterExists(String category, String counter) in \src\Logary.WinPerfCounters\WinPerfCounter.fs:line 81
*)
/// Checks whether the counter in the category exists
let counterExists category counter =
  try
    PCC.CounterExists(counter, category)
  with
  | :? InvalidOperationException as e when e.Message.Contains("does not exist") ->
    false

/// Gets a list of performance counters for the given instance and category.
let getCounters (pcc : PCC) (instance : Instance) : _ list =
  try
    match instance with
    | NotApplicable     ->
      try pcc.GetCounters ()
      // I haven't found a way to check this properly; not all categories return
      // errors like this:
      // System.ArgumentException: Counter is not single instance, an instance name needs to be specified.
      with :? ArgumentException -> Array.empty
    | Instance instance -> pcc.GetCounters instance
    |> Array.map (fun pc -> { category = pcc.CategoryName
                              counter  = pc.CounterName
                              instance = instance })
    |> Array.toList
  with
  | :? InvalidOperationException ->
    // instance has gone away, e.g. if a thread is a perf counter instance but
    // terminates before calling 'getCounters' on its perf counter instance.
    // > System.InvalidOperationException: Instance devenv/61 does not exist in category Thread.
    []

let getAllCounters () =
  getAllPCC ()
  |> List.map (fun pcc -> pcc, getInstances pcc)
  |> List.map (fun (pcc, instances) ->
    pcc, (instances |> List.map (fun inst -> inst, getCounters pcc inst)))

/// Gets the next value for the performance counter
let nextValue (pc : PC) =
  pc.NextValue() |> float

/// Create a new performance counter given a WindowsPerfCounter record.
let toPC { category = cat; counter = cnt; instance = inst } : PC option =
  if counterExists cat cnt then
    match inst with
    | Instance inst when true -> //this is bugged on mono: instanceExists cat inst ->
      let pc = new PerformanceCounter(cat, cnt, inst, true)
      Some pc
    | _ ->
      let pcc = getPCC cat |> Option.get
      match pcc.CategoryType with
      | PCCT.MultiInstance ->
        // this perf counter category is multi-instance, and yet no instance has
        // been given, which most likely means the call-site of toPC didn't find
        // any instances in thie PCC. It's possible that in the future this PCC
        // contains an instance, but until then, we can't create a perf counter
        // from it.
        None
      | PCCT.Unknown | PCCT.SingleInstance ->
        new PerformanceCounter(cat, cnt, "", true) |> Some
      | typ -> failwithf "unknown type %A" typ
  else
    None

/// Curried variant of `mkPc` that takes a category, counter and optional
/// instance and creates an `Option<PerformanceCounter>` from it.
let toPC' category counter instance =
  toPC { category = category; counter = counter; instance = instance }

/// try to find the instance performance counter for the pid, or return
/// NotApplicable if the process e.g. does no longer run and can therefore not
/// be found
let pidToInstance pid =
  getPCC "Process"
  |> Option.get
  |> getInstances
  |> List.map (fun instance ->
    match toPC' "Process" "ID Process" instance with
    | Some pcProcId when int (nextValue pcProcId) = pid ->
      pcProcId.InstanceName
    | _ -> "")
  |> List.tryFind (fun s -> s.Length > 0)
  |> Option.fold (fun _ t -> Instance t) NotApplicable

/// Gets the current process' id
let pid () =
  Process.GetCurrentProcess().Id

/// Gets the performance counter instance for the given category for the current
/// process.
let pidInstance () =
  pidToInstance (pid ())

/// Sets the Performance Counter to only check metrics that are sliced to be for
/// the current process.
let setCurrentProcess pc =
  { pc with instance = pidToInstance (pid ()) }

/// Sets a specific instance on the Performance Counter.
let setInstance (i : Instance) pc =
  { pc with instance = i }

module PointName =
  open Logary

  let ofPerfCounter (c : PerfCounter) =
    let nameInstance instance =
      match instance with
      | NotApplicable -> [| c.category; c.counter |]
      | Instance inst -> [| c.category; c.counter; inst |]

    PointName (nameInstance c.instance)

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module PerfCounter =
  open Logary.Internals

  let private ofPerfCounter =
    Cache.memoize PointName.ofPerfCounter

  let toValue ((perfCounter, pc) : PerfCounter * PC) =
    Float (nextValue pc)
    |> Message.derivedWithUnit (ofPerfCounter perfCounter) Units.Scalar