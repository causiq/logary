module Logary.Metrics.SQLServerHealth

open System
open System.IO
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

module StringUtils =
  let camelToSnake (str : string) =
    let rec r cs = function
      | curr when curr = str.Length ->
        new String(cs |> List.rev |> List.toArray)
      | curr when Char.IsUpper str.[curr] && curr = 0 ->
        r ((Char.ToLower str.[curr]) :: cs) (curr + 1)
      | curr when Char.IsUpper str.[curr] ->
        r ((Char.ToLower str.[curr]) :: '_' :: cs) (curr + 1)
      | curr ->
        r (str.[curr] :: cs) (curr + 1)
    r [] 0

module Database =
  open System.Text.RegularExpressions

  open FsSql

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

  type IOInfo =
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

  let delta earlier later =
    if earlier.filePath <> later.filePath then invalidArg "later" "comparing different db files"
    { ioStallReadMs     = later.ioStallReadMs - earlier.ioStallReadMs
      ioStallWriteMs    = later.ioStallWriteMs - earlier.ioStallWriteMs
      ioStall           = later.ioStall - earlier.ioStall
      numOfReads        = later.numOfReads - earlier.numOfReads
      numOfWrites       = later.numOfWrites - earlier.numOfWrites
      numOfBytesRead    = later.numOfBytesRead - earlier.numOfBytesRead
      numOfBytesWritten = later.numOfBytesWritten - earlier.numOfBytesWritten
      drive             = later.drive
      dbName            = later.dbName
      filePath          = later.filePath
      fileType          = later.fileType }

  let ioInfo connMgr =
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
    |> Sql.map (Sql.asRecord<IOInfo> "")

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module IOInfo =
  open Database

  let inline zeroDiv den nom =
    if nom = 0L then 0. else float den / float nom

  let readLatency data =
    zeroDiv data.ioStallReadMs data.numOfReads

  let writeLatency data =
    zeroDiv data.ioStallWriteMs data.numOfWrites

  let latency data =
    zeroDiv data.ioStall (data.numOfReads + data.numOfWrites)

  let bytesPerRead data =
    zeroDiv data.numOfBytesRead data.numOfReads

  let bytesPerWrite data =
    zeroDiv data.numOfBytesWritten data.numOfWrites

  let bytesPerTransfer data =
    zeroDiv (data.numOfBytesRead + data.numOfBytesWritten)
            (data.numOfReads + data.numOfWrites)

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
  open NodaTime

  open Logary.Internals
  open Database

  let dotToUnderscore (s : string) =
    s.Replace(".", "_")

  type State =
    { connMgr  : Sql.ConnectionManager
      lastIOs  : IOInfo list * Instant
      deltaIOs : IOInfo list * Duration
      dps      : DP list }

  type DPCalculator = IOInfo -> Measure

  let datapoints =
    [ "io_stall_read", fun io -> Measure.fromInt64 "io_stall_read" (Units.Time Milliseconds) io.ioStallReadMs
      "io_stall_write", fun io -> Measure.fromInt64 "io_stall_write" (Units.Time Milliseconds) io.ioStallWriteMs 
      "io_stall", fun io -> Measure.fromInt64 "io_stall" (Units.Time Milliseconds) io.ioStall
      "num_of_reads", fun io -> Measure.fromInt64 "num_of_reads" (Units.Unit "reads") io.numOfReads
      "num_of_writes", fun io -> Measure.fromInt64 "num_of_writes" (Units.Unit "writes") io.numOfWrites
      "num_of_bytes_read", fun io -> Measure.fromInt64 "num_of_bytes_read" Units.Bytes io.numOfBytesRead 
      "num_of_bytes_written", fun io ->  Measure.fromInt64 "num_of_bytes_read" Units.Bytes io.numOfBytesRead
      "read_latency", IOInfo.readLatency >> Measure.fromFloat "read_latency" (Units.Time Milliseconds)
       ]
    |> Map.ofList : Map<string, DPCalculator>

  /// ns: logary.sql_server_health
  /// prefix: drive | file (metric name prefix)
  /// instance: intelliplan.mdf
  let dps ns prefix instance =
    datapoints |> Seq.map (fun kv -> kv.Key) |> Seq.toList
    |> List.map (fun metric -> sprintf "%s.%s_%s.%s" ns prefix metric instance)


  // example data point, collected for a Duration

  // ns.category.perf_counter.instance

  // logary.sql_server_health.file_io_stall_read.

  // ns    .component        .metric_name       .instance.calculated
  // logary.sql_server_health.drive_latency_read.c
  // logary.sql_server_health.drive_latency_read.c       .mean // (reservoir)
  // logary.sql_server_health.drive_latency_read.c       .max // (reservoir)
  // logary.sql_server_health.cpu_queue_len.max // (reservoir)

  // guage shaped, polled ever n ms
  // logary.sql_server_health.drive_latency_read.c
  // logary.sql_server_health.db_latency_read   .intelliplan.read

  // histogram shaped, values added every n ms
  // logary.sql_server_health.drive_latency.read_ms.c.mean
  // logary.sql_server_health.drive_latency_read.c.max
  // logary.sql_server_health.drive_latency_read.c.p99
  // logary.sql_server_health.drive_latency_read.c.p75

  let loop (conf : Conf) (ri : RuntimeInfo) (inbox : IActor<_>) =
    let log = LogLine.setPath "Logary.Metrics.SQLServerHealth"
              >> Logger.log ri.logger

    let driveDps = dps "logary.sql_server_health" "drive"
    let fileDps  = Path.GetFileName >> dotToUnderscore
                   >> dps "logary.sql_server_health" "file"

    let rec init () = async {
      LogLine.info "init" |> log
      let connMgr = Sql.withNewConnection conf.openConn
      let measures = ioInfo connMgr |> Seq.toList
      let driveDps    = measures |> List.map (fun l -> l.drive |> driveDps) |> List.concat
      let fileDps     = measures |> List.map (fun l -> l.filePath |> fileDps) |> List.concat
      return! running { connMgr  = connMgr
                        lastIOs  = measures, Date.now ()
                        deltaIOs = [], Duration.Epsilon
                        dps      = List.map DP (driveDps @ fileDps) }
      }

    and running ({ lastIOs = lastIOMeasures, lastIOInstant } as state) = async {
      let! msg, _ = inbox.Receive()
      match msg with
      | GetValue (dps, replChan) ->
        LogLine.info "get value" |> log
        replChan.Reply []
        return! running state

      | GetDataPoints replChan ->
        LogLine.info "get dps" |> log
        replChan.Reply state.dps
        return! running state

      | Update msr ->
        LogLine.infof "does not support updating path %s" msr.m_path |> log
        return! running state

      | Sample ->
        LogLine.info "sample" |> log
        let now    = Date.now ()
        let curr   = ioInfo state.connMgr |> List.ofSeq
        let dio    = (lastIOMeasures, curr) ||> List.map2 delta
        let dioDur = now - lastIOInstant
        return! running { state with lastIOs = curr, now
                                     deltaIOs = dio, dioDur }

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

