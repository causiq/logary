module Logary.Metrics.SQLServerHealth

open System
open System.Data

open FSharp.Actor

open Logary
open Logary.Internals
open Logary.Metric
open Logary.AsmUtils

type DatabaseName = string
type FullyQualifiedPath = string
type FileType =
  | DataFile = 1
  | LogFile  = 2
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

module Database =
  open FsSql

  let internal P = Sql.Parameter.make

  type PLE =
    { serverName         : string
      objectName         : string
      instanceName       : string
      pageLifeExpectancy : int64 }

  /// PLE is a good measurement of memory pressure.
  /// Higher PLE is better. Watch the trend over time, not the absolute value.
  /// This will only return one row for non-NUMA systems.
  /// 
  /// Page Life Expectancy (PLE) value for each NUMA node in current instance
  /// (Query 35) (PLE by NUMA Node)
  let ple connMgr =
    let sql = "
SELECT
  @@SERVERNAME  AS serverName,
  [object_name] AS objectName,
  instance_name AS instanceName,
  cntr_value    AS pageLifeExpectancy
FROM sys.dm_os_performance_counters WITH (NOLOCK)
WHERE [object_name] LIKE N'%Buffer Node%' -- Handles named instances
  AND counter_name = N'Page life expectancy' OPTION (RECOMPILE)"
    Sql.execReader connMgr sql []
    |> Sql.mapOne (Sql.asRecord<PLE> "")
    |> fun ple ->
      { ple with objectName   = ple.objectName.TrimEnd()
                 instanceName = ple.instanceName.TrimEnd() }

  type LatencyDetails =
    { ioStallReadMs     : int64
      ioStallWriteMs    : int64
      ioStall           : int64
      numOfReads        : int64
      numOfWrites       : int64
      numOfBytesRead    : int64
      numOfBytesWritten : int64
      drive             : DriveName
      dbName            : DatabaseName
      filePath          : FullyQualifiedPath
      fileType          : FileType }

  let latencyInfo connMgr =
    let sql = "
SELECT
  [io_stall_read_ms]             AS ioStallReadMs,
  [io_stall_write_ms]            AS ioStallWriteMs,
  [io_stall]                     AS ioStall,
  [num_of_reads]                 AS numOfReads,
  [num_of_writes]                AS numOfWrites,
  [num_of_bytes_read]            AS numOfBytesRead,
  [num_of_bytes_written]         AS numOfBytesWritten,
  LEFT ([mf].[physical_name], 2) AS drive,
  DB_NAME ([vfs].[database_id])  AS dbName,
  [mf].[physical_name]           AS filePath,
  [vfs].[file_id]                AS fileType
FROM
  sys.dm_io_virtual_file_stats (NULL, NULL) AS [vfs]
JOIN sys.master_files AS [mf]
  ON [vfs].[database_id] = [mf].[database_id]
  AND [vfs].[file_id] = [mf].[file_id]"
    Sql.execReader connMgr sql []
    |> Sql.map (Sql.asRecord<LatencyDetails> "")

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module LatencyDetails =
  open Database

  let readLatency data =
    if data.numOfReads = 0L then 0. else
    float data.ioStallReadMs / float data.numOfReads

  let writeLatency data =
    if data.numOfWrites = 0L then 0. else
    float data.ioStallWriteMs / float data.numOfWrites

  let latency data =
    if data.numOfWrites = 0L && data.numOfReads = 0L then 0. else
    float data.ioStall / float (data.numOfReads + data.numOfWrites)



type Conf =
  { /// Getting the contents of an embedded resource file function.
    contentsOf     : ResourceName -> string
    /// A list of probe data sources
    latencyTargets : LatencyProbeDataSources list
    /// Connection factory function
    openConn       : unit -> IDbConnection }

/// default config
let empty =
  { contentsOf     = readResource
    latencyTargets = []
    openConn       = fun () -> failwith "connection factory needs to be provided" }

module private Impl =

  type State =
    { connMgr    : Sql.ConnectionManager }

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

