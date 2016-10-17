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
    category.GetInstanceNames()

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
  { category  : string
    counter   : string
    instances : Set<string>
    unit      : Units option }

  member x.baseName =
    PointName [| x.category; x.counter |]

  member x.findCategory () =
    Category.create x.category

  /// Creates a new WinPerfCounter from the passed strings.
  static member create(category, counter, instances : string seq) =
    { category  = category
      counter   = counter
      instances = Set.ofSeq instances
      unit      = None }

  /// Creates a new WinPerfCounter from the passed strings.
  static member create(category, counter, instances, units) =
    { category  = category
      counter   = counter
      instances = instances
      unit      = units }

type WinPerfCounterInstance =
  { category  : Category
    counter   : PerformanceCounter
    instances : string []
    unit      : Units
    baseName  : PointName }

  /// This W P C is 'singleton' and doesn't have neither a 'global single instance' nor 'instances'.
  member x.isNotInstanceBased =
    Array.isEmpty x.instances && x.counter.CounterName <> ""

  member x.isSingleInstance =
    x.counter.CounterName = ""

  member x.isMultiInstance =
    not (Array.isEmpty x.instances)

  /// Only call when instances are one or zero in count.
  member x.nextValue () =
    if x.instances = [||] then
      x.baseName,
      float (x.counter.NextValue())
    elif Array.length x.instances = 1 then
      x.baseName |> PointName.setEnding x.instances.[0],
      float (x.counter.NextValue())
    else
      failwithf "Cannot get single value for %O with there are %i instances available"
                x.baseName x.instances.Length

  /// Get the instance-value, value -pairs for this Windows Performance Counter instance.
  /// The PointNames will only contain the instance names, not the base name.
  member x.nextValues () =
    x.instances
    |> Array.map (fun instance ->
      x.counter.InstanceName <- instance
      let sample = x.counter.NextSample()
      printfn "%A" sample
      instance,
      Float (float (x.counter.NextValue()))
    )

  static member create(category, counter, instances : string seq, units) =
    { category  = category
      counter   = counter
      instances = Array.ofSeq instances
      unit      = defaultArg units Scalar
      baseName  = PointName [| category.CategoryName; counter.CounterName |] }

module PointName =
  open Logary

  let ofPerfCounter (counter : WinPerfCounter) =
    PointName [| counter.category; counter.counter |]

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

    let All = [| _Total; _Global_; Default |]

  /// Gets a list of performance counters for the given instance and category.
  let list (pcc : Category) (instances : string seq) : WinPerfCounterInstance [] =
    try
      match Array.ofSeq instances with
      | [||] ->
        try
          pcc.GetCounters ()
          |> Array.map (fun c -> WinPerfCounterInstance.create(pcc, c, Set.empty, None))
        // I haven't found a way to check this properly; not all categories return
        // errors like this:
        // System.ArgumentException: Counter is not single instance, an instance name needs to be specified.
        with :? ArgumentException ->
          Array.empty

      | instances ->
        let i2cf m inst =
          match m |> Map.tryFind inst with
          | None   -> m |> Map.add inst (pcc.GetCounters inst)
          | Some _ -> m

        // (instance to counters) map
        let i2c = instances |> Array.fold i2cf Map.empty

        // (inst -> counter list) map to (counter -> instance list) map
        let c2if m inst ctrs : Map<string, PerformanceCounter * string list> =

          let c2ifInner (m : Map<string, PerformanceCounter * string list>) (counter : PerformanceCounter) =
            match m |> Map.tryFind counter.CounterName with
            | None ->
              m |> Map.add counter.CounterName (counter, [ inst ])
            | Some (counter, xinstances) ->
              m |> Map. add counter.CounterName (counter, (inst :: xinstances))

          ctrs |> Array.fold c2ifInner m

        let c2i = i2c |> Map.fold c2if Map.empty

        c2i
        |> Seq.map (fun (KeyValue (_, (counter, instances))) ->
          WinPerfCounterInstance.create(pcc, counter, instances, None))
        |> Array.ofSeq

    with
    | :? InvalidOperationException ->
      // instance has gone away, e.g. if a thread is a perf counter instance but
      // terminates before calling 'getCounters' on its perf counter instance.
      // > System.InvalidOperationException: Instance devenv/61 does not exist in category Thread.
      Array.empty

  /// Create a new performance counter given a WinPerfCounter.
  let toWindowsCounter (counter : WinPerfCounter) : WinPerfCounterInstance option =
    if Category.exists counter.category counter.counter then
      let category = Category.createForce counter.category

      match counter.instances with
      | instances when not (Set.isEmpty instances) ->
        WinPerfCounterInstance.create (
          category,
          new PerformanceCounter(counter.category, counter.counter, "__CHANGE_ON_QUERY__", (* read only *) true),
          instances,
          counter.unit)
        |> Some

      | _ ->
        match category.CategoryType with
        | CategoryType.MultiInstance ->
          // this perf counter category is multi-instance, and yet no instance has
          // been given, which most likely means the call-site of toWindowsCounter didn't find
          // any instances in thie PCC. It's possible that in the future this Category
          // contains an instance, but until then, we can't create a perf counter
          // from it.
          None

        | CategoryType.Unknown
        | CategoryType.SingleInstance ->
          WinPerfCounterInstance.create (
            category,
            new PerformanceCounter(counter.category, counter.counter, "", (* read only *) true),
            [],
            counter.unit)
        |> Some

        | typ ->
          failwithf "unknown type %A" typ
    else
      None

  /// Curried variant of `mkPc` that takes a category, counter and optional
  /// instance and creates an `Option<PerformanceCounter>` from it.
  let toWindowsCounter3 category counter instance =
    WinPerfCounter.create(category, counter, instance)
    |> toWindowsCounter

  /// Sets a specific unit on the WinPerfCounter.
  let setUnit (units : Units) (counter : WinPerfCounter) =
    { counter with unit = Some units }

  module Helpers =

    let toValue (pc : WinPerfCounterInstance) =
      let message = Message.gaugeWithUnit pc.baseName pc.unit (Int64 1L)
      pc.nextValues()
      |> Array.fold (fun m (pn, vl) ->
          let field = Field (vl, Some pc.unit)
          m |> Message.setFieldValue pn field)
        message

    let getAllCounters () =
      Category.list ()
      |> Array.map (fun category -> category, Category.instances category)
      |> Array.map (fun (category, instances) -> category, list category instances)

    /// Try to find the instance performance counter for the pid, or return
    /// None if the process e.g. does no longer run and can therefore not
    /// be found.
    let pidToInstance pid : string option =
      Category.createForce "Process"
      |> Category.instances
      |> Array.map (fun instance ->
        match toWindowsCounter3 "Process" "ID Process" [ instance ] with
        | Some pcProcId when int (snd (pcProcId.nextValue())) = pid ->
          pcProcId.instances.[0]
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
    let scopeToProcess (counter : WinPerfCounter) : WinPerfCounter option =
      let pid = pid ()
      pidToInstance pid
      |> Option.map (fun inst -> { counter with instances = set [ inst ] })

    // NOTE: Windows has a bug where the pid/instance index changes during runtime
    // if you have more than a single process with the same name and at least one
    // of those processes exists.