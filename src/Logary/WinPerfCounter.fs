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

/// Type alias System.Diagnostics.for PerformanceCounterCategory
type PCC = PerformanceCounterCategory

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

/// Checks whether the counter in the category exists
let counterExists category counter =
  PCC.CounterExists(counter, category)

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
    | Instance inst when instanceExists cat inst ->
      new PerformanceCounter(cat, cnt, inst, true) |> Some
    | _ ->
      new PerformanceCounter(cat, cnt, "", true) |> Some
  else
    None

/// Curried variant of `mkPc` that takes a category, counter and optional
/// instance and creates an `Option<PerformanceCounter>` from it.
let toPC' category counter instance =
  toPC { category = category; counter = counter; instance = instance }

/// try to find the instance performance counter for the pid, or return
/// NotApplicable if the process e.g. does no longer run and can therefore not
/// be found
let pidToInstance category pid =
  match getPCC category with
  | None ->
    NotApplicable
  | Some cat ->
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
let pidInstance category =
  pidToInstance category (pid ())

/// Sets the Performance Counter to only check metrics that are sliced to be for
/// the current process.
let setCurrentProcess pc =
  { pc with instance = pidToInstance pc.category (pid ()) }

/// Sets a specific instance on the Performance Counter.
let setInstance (i : Instance) pc =
  { pc with instance = i }

module private Gen =
  open System.Text.RegularExpressions

  let munge name =
    Regex.Replace(name, "[\./]", "_")

  /// SynchronizationNuma: A nice help text
  ///
  /// This performance counter does not have non-instance based counters
  module ``SynchronizationNuma Example`` =

    [<Literal>]
    let Category = "SynchronizationNuma"

    let PCC = getPCC Category

    let instances () =
      PCC |> Option.fold (fun s pcc -> getInstances pcc) []

    let ``Exec. Resource no-Waits AcqShrdWaitForExcl/sec`` instance =
      toPC' Category "Exec. Resource no-Waits AcqShrdWaitForExcl/sec" instance

    let ``Exec. Resource Boost Excl. Owner/sec`` instance =
      toPC' Category "Exec. Resource Boost Excl. Owner/sec" instance

    // etc

    let allCounters =
      [ ``Exec. Resource no-Waits AcqShrdWaitForExcl/sec``
        ``Exec. Resource Boost Excl. Owner/sec``
        // etc
      ]

    let countersFor instance =
      allCounters
      |> List.map (fun f -> f instance)
      |> List.filter Option.isSome
      |> List.map Option.get

  /// System: a nice help text here too
  ///
  /// This performance counter does not have instance based counters
  module ``System Example`` =

    [<Literal>]
    let Category = "System"

    let PCC = getPCC Category

    let ``File Read Operations/sec`` =
      { category = Category; counter = "File Read Operations/sec"; instance = NotApplicable }

    // etc

    let allCounters =
      [ ``File Read Operations/sec`` ]

  let gen () =

    let genComment (pcc : PCC) { instance = i } =
      sprintf
         "/// %s: %s
///
/// %s"
        pcc.CategoryName
        pcc.CategoryHelp
        (match i with
        | NotApplicable -> "This performance counter does not have instance based counters"
        | _             -> "This performance counter does not have non-instance based counters")

    let genModuleHeader (pcc : PCC) =
      sprintf """module ``%s`` =

  [<Literal>]
  let Category = "%s"

  let PCC = getPCC Category"""
        pcc.CategoryName
        pcc.CategoryName

    let genCounter pc =
      let osPC = toPC pc |> Option.get
      match pc with
      | { category = cat; counter = cnt; instance = NotApplicable } ->
        sprintf """  /// %s: %s
  let ``%s`` =
    %s"""
          osPC.CounterName osPC.CounterHelp osPC.CounterName
          (sprintf """{ category = "%s"; counter = "%s"; instance = NotApplicable }"""
             cat cnt)
      |  { category = cat; counter = cnt } ->
        sprintf """  /// %s: %s
  let ``%s`` instance =
    %s"""
          osPC.CounterName osPC.CounterHelp osPC.CounterName
          (sprintf """{ category = "%s"; counter = "%s"; instance = instance }"""
             cat cnt)

    let genCounters (counters : PerfCounter list) =
      counters
      |> List.map genCounter
      |> fun ctrs -> String.Join("\n", ctrs)

    let genListing (counters : PerfCounter list) =
      match counters with
      | [] -> """  let allCounters = []"""
      | hc :: rest ->
        sprintf """
  let allCounters =
    [ ``%s``
%s
    ]"""
          hc.counter
          (rest
           |> List.map (fun { counter = c } -> "``" + c + "``")
           |> List.map (fun s -> "      " + s)
           |> fun ss -> String.Join("\n", ss))

    getAllPCC ()
    |> List.map (fun pcc -> pcc, getInstances pcc)
    |> List.sortBy (fun (pcc, _) -> pcc.CategoryName)
    |> List.map (function
      | pcc, []        -> pcc, getCounters pcc NotApplicable
      | pcc, inst :: _ -> pcc, getCounters pcc inst)
    |> List.map (fun (pcc, (c :: rest as counters)) ->
      genComment pcc c
      + (genModuleHeader pcc)
      + (genCounters counters)
      + (genListing counters))
    |> fun modules ->
      String.Join("\n", modules)