module Logary.Metrics.SQLServerHealth

open FSharp.Actor

open Logary
open Logary.Internals
open Logary.Metric
open Logary.AsmUtils

type DatabaseName = string
type FullyQualifiedPath = string
type DriveName = string

/// A discriminated union describing what sorts of things the probe should fetch
/// data from.
type LatencyProbeDataSources =
  /// A single file is the lowest qualifier that gives unique latency results
  | SingleFile of FullyQualifiedPath
  /// A database consists of a single or many DataFiles and a single or many
  /// LogFiles.
  | Database of DatabaseName
  /// E.g. 'C:' or 'D:' - usually all things on the drive have similar performance
  /// metrics as it's the underlying device that sets the constraints. Do not
  /// include the backslash in this name.
  | Drive of DriveName

type Conf =
  { /// Getting the contents of an embedded resource file function.
    contentsOf     : ResourceName -> string
    /// A list of probe data sources
    latencyTargets : LatencyProbeDataSources list }

/// default config
let empty =
  { contentsOf     = readResource
    latencyTargets = [] }

module private Impl =

  type State =
    { lastLatencies : obj
      lastStalls    : obj
      lastPLE       : obj }

  // example data point, collected for a Duration

  // ns.category.perf_counter.instance
  // ns    .component        .metric_name       .instance.calculated
  // logary.sql_server_health.drive_latency_read.c
  // logary.sql_server_health.drive_latency_read.c       .mean // (reservoir)
  // logary.sql_server_health.drive_latency_read.c       .max // (reservoir)
  // logary.sql_server_health.cpu_queue_len.max // (reservoir)

  // guage shaped, polled ever n ms
  // logary.sql_server_health.drive_latency_read.c
  // logary.sql_server_health.db_latency_read   .intelliplan.read

  let baseName = "logary.sql_server_health"
  let driveLatency drive = String.concat "." [baseName; "drive_latency"; drive]
  let fileLatency drive = String.concat "." [baseName; "dbfile_latency"; drive]

  // histogram shaped, values added every n ms
  // logary.sql_server_health.drive_latency.c.read_ms.mean
  // logary.sql_server_health.drive_latency.c.read_ms.max
  // logary.sql_server_health.drive_latency.c.read_ms.p99
  // logary.sql_server_health.drive_latency.c.read_ms.p75

  let loop (conf : Conf) (ri : RuntimeInfo) (inbox : IActor<_>) =
    let log = LogLine.setPath "Logary.Metrics.SQLServerHealth"
              >> Logger.log ri.logger

    let rec init () = async {
      LogLine.info "init" |> log
      return! running ()
      }

    and running state = async {
      let! msg, _ = inbox.Receive()
      match msg with
      | GetValue (dps, replChan) ->
        LogLine.info "get value" |> log
        replChan.Reply []
        return! running ()

      | GetDataPoints replChan ->
        LogLine.info "get dps" |> log
        let res = [] // TODO: need to query to get baseline drives and files
          
        replChan.Reply res
        return! running ()

      | Update msr ->
        match msr.m_value'' with
        | Some v -> return! running ()
        | None   -> return! running ()

      | Sample ->
        LogLine.info "sample" |> log
        return! running state

      | Shutdown ackChan ->
        ackChan.Reply Ack
        return! shutdown state

      | Reset ->
        return! reset state
      }

    and reset state = async {
      LogLine.info "reset" |> log
      return! init ()
      }

    and shutdown state = async {
      LogLine.info "shutdown" |> log
      return ()
      }

    init ()

/// Create the new SQLServer Health metric
let create conf = MetricUtils.stdNamedMetric Probe (Impl.loop conf)

/// C# interop: Create a new Noop metric that doesn't do very much
[<CompiledName "Create">]
let create' (conf, name) =
  create conf name

