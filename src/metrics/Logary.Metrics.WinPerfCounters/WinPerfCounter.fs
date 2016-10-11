// see http://www.mono-project.com/Mono_Performance_Counters
// see http://www.databasejournal.com/features/mssql/article.php/3932406/Top-10-SQL-Server-Counters-for-Monitoring-SQL-Server-Performance.htm
// http://www.quest.com/backstage/images/promotions/SQLServer-Perfmonance-Poster.pdf
// http://www.mssqltips.com/sqlservertip/2460/perfmon-counters-to-identify-sql-server-disk-bottlenecks/
// http://www.mssqltips.com/sqlservertip/1265/sql-server-database-specific-performance-counters/
// http://matt.aimonetti.net/posts/2013/06/26/practical-guide-to-graphite-monitoring/
// https://stackoverflow.com/questions/4455187/wrong-calculated-cpu-usage-using-c-sharp-and-wmi

/// A module that helps you interact with Windows Performance Counters. Wraps
/// null-based APIs and guides the programmer with sane names and documentation.
namespace Logary.Metrics

open System
open System.Diagnostics
open Logary
open Logary.Internals

/// Type alias System.Diagnostics.for PerformanceCounterCategory
type Category = PerformanceCounterCategory

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Category =

  /// Create a new performance counter category. It may not exist on the system
  /// that you are running the code on; hence the option-based return value
  let create (name : string) : Category option =
    if Category.Exists name then
      Some (Category name)
    else
      None

  let createForce (name : string) : Category =
    match create name with
    | None ->
      failwithf "Failed to create Category '%s'" name
    | Some category ->
      category

  /// Gets all available performance counter categories
  let list () : Category [] =
    Category.GetCategories()

  /// Gets all available instance names for the given list of performance counter
  /// categories
  let instances (category : Category) : _ [] =
    category.GetInstanceNames() |> Array.map Some

  /// Checks whether the instance exists in the category
  let instanceExists (category : string) (instance : string) =
    Category.InstanceExists(instance, category)

  /// Checks whether the counter in the category exists
  let exists category counter =
    try
      Category.CounterExists(counter, category)
    with
    | :? InvalidOperationException as e when e.Message.Contains("does not exist") ->
      false
  (*
  Reason for try-with:

  System.InvalidOperationException: Category does not exist.
     at System.Diagnostics.PerformanceCounterLib.CounterExists(String machine, String category, String counter)
     at System.Diagnostics.PerformanceCounterCategory.CounterExists(String counterName, String categoryName, String machineName)
     at System.Diagnostics.PerformanceCounterCategory.CounterExists(String counterName, String categoryName)
     at Logary.Metrics.WinPerfCounter.counterExists(String category, String counter) in \src\Logary.WinPerfCounters\WinPerfCounter.fs:line 81
  *)

/// Type alias for System.Diagnostics.PerformanceCounterCategoryType
type CategoryType = PerformanceCounterCategoryType

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
type WinPerfCounter =
  { category : string
    counter  : string
    instance : string option }

  member x.findCategory () =
    Category.create x.category

  /// Creates a new WinPerfCounter from the passed strings.
  static member create(category, counter, instance : string option) =
    { category = category
      counter  = counter
      instance = instance }

module PointName =
  open Logary

  let ofPerfCounter (counter : WinPerfCounter) =
    PointName <|
      match counter.instance with
      | None ->
        [| counter.category; counter.counter |]
      | Some inst ->
        [| counter.category; counter.counter; inst |]

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module WinPerfCounter =

  module KnownInstances =
    /// The instance that is the sum of all instances. It's the literal `_Total`.
    [<Literal>]
    let _Total = "_Total"

    /// The instance that runs on operating system level.
    [<Literal>]
    let _Global_ = "_Global_"

    [<Literal>]
    let Default = "Default"

  /// Gets a list of performance counters for the given instance and category.
  let list (pcc : Category) (instance : string option) : WinPerfCounter [] =
    try
      match instance with
      | None ->
        try pcc.GetCounters ()
        // I haven't found a way to check this properly; not all categories return
        // errors like this:
        // System.ArgumentException: Counter is not single instance, an instance name needs to be specified.
        with :? ArgumentException ->
          Array.empty

      | Some instance -> pcc.GetCounters instance
      |> Array.map (fun pc ->
        WinPerfCounter.create (pcc.CategoryName, pc.CounterName, instance))

    with
    | :? InvalidOperationException ->
      // instance has gone away, e.g. if a thread is a perf counter instance but
      // terminates before calling 'getCounters' on its perf counter instance.
      // > System.InvalidOperationException: Instance devenv/61 does not exist in category Thread.
      Array.empty

  /// Create a new performance counter given a WinPerfCounter.
  let toWindowsCounter (counter : WinPerfCounter) : PerformanceCounter option =
    if Category.exists counter.category counter.counter then
      match counter.instance with
      | Some inst when true -> // this is bugged on mono: instanceExists cat inst ->
        let pc = new PerformanceCounter(counter.category, counter.counter, inst, true)
        Some pc

      | _ ->
        let category = counter.findCategory() |> Option.get
        match category.CategoryType with
        | CategoryType.MultiInstance ->
          // this perf counter category is multi-instance, and yet no instance has
          // been given, which most likely means the call-site of toPC didn't find
          // any instances in thie PCC. It's possible that in the future this PCC
          // contains an instance, but until then, we can't create a perf counter
          // from it.
          None

        | CategoryType.Unknown
        | CategoryType.SingleInstance ->
          new PerformanceCounter(counter.category, counter.counter, "", true) |> Some

        | typ ->
          failwithf "unknown type %A" typ
    else
      None

  /// Curried variant of `mkPc` that takes a category, counter and optional
  /// instance and creates an `Option<PerformanceCounter>` from it.
  let toWindowsCounter3 category counter instance =
    WinPerfCounter.create(category, counter, instance)
    |> toWindowsCounter

  /// Sets a specific instance on the WinPerfCounter.
  let setInstance (instance : string option ) (counter : WinPerfCounter) =
    { counter with instance = instance }

  module Helpers =

    let toValue ((perfCounter, pc) : WinPerfCounter * PerformanceCounter) =
      Float (float (pc.NextValue()))
      |> Message.derivedWithUnit (PointName.ofPerfCounter perfCounter) Units.Scalar


    let getAllCounters () =
      Category.list ()
      |> Array.map (fun category -> category, Category.instances category)
      |> Array.map (fun (category, instances) ->
        category, (instances |> Array.map (fun instance -> instance, list category instance)))

    /// Try to find the instance performance counter for the pid, or return
    /// None if the process e.g. does no longer run and can therefore not
    /// be found.
    let pidToInstance pid : string option =
      Category.createForce "Process"
      |> Category.instances
      |> Array.map (fun instance ->
        match toWindowsCounter3 "Process" "ID Process" instance with
        | Some pcProcId when int (pcProcId.NextValue()) = pid ->
          pcProcId.InstanceName
        | _ ->
          "")
      |> Array.tryFind (fun s -> s.Length > 0)
      |> Option.fold (fun _ t -> Some t) None

    /// Gets the current process' id
    let pid () : int =
      Process.GetCurrentProcess().Id

    /// Gets the performance counter instance for the given category for the current
    /// process.
    let pidInstance () =
      pidToInstance (pid ())

    /// Sets the Performance Counter to only check metrics that are sliced to be for
    /// the current process.
    let setCurrentProcess (counter : WinPerfCounter) : WinPerfCounter =
      { counter with instance = pidToInstance (pid ()) }

    // NOTE: Windows has a bug where the pid/instance index changes during runtime
    // if you have more than a single process with the same name and at least one
    // of those processes exists.