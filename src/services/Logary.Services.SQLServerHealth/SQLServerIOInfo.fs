module Logary.Metrics.SQLServerIOInfo

open System
open System.IO
open System.Data

open Hopac

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

  let sumBy data f =
    match data |> List.filter f with
    | []            -> failwithf "filter returned no results"
    | first :: rest ->
      rest |> List.fold (fun (s : IOInfo) t ->
        { s with
            ioStallReadMs     = s.ioStallReadMs + t.ioStallReadMs
            ioStallWriteMs    = s.ioStallWriteMs + t.ioStallWriteMs
            ioStall           = s.ioStall + t.ioStall
            numOfReads        = s.numOfReads + t.numOfReads
            numOfWrites       = s.numOfWrites + t.numOfWrites
            numOfBytesRead    = s.numOfBytesRead + t.numOfBytesRead
            numOfBytesWritten = s.numOfBytesWritten + t.numOfBytesWritten })
        first

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

module internal Impl =
  open NodaTime

  open Logary.Internals
  open Database

  let dotToUnderscore (s : string) =
    s.Replace(".", "_")

  let removeColon (s : string) =
    s.Replace(":", "")

  type State =
    { connMgr     : Sql.ConnectionManager
      lastIOs     : IOInfo list * Instant
      deltaIOs    : IOInfo list * Duration
      dps         : DP list
      sampleFresh : bool }

  type DPCalculator = IOInfo -> DP -> Measure

  let datapoints =
    let fromInt64 u v dp = Measure.fromInt64 dp u v
    let fromFloat u v dp = Measure.fromFloat dp u v
    [ "io_stall_read", fun io -> fromInt64 (Units.Time Milliseconds) io.ioStallReadMs
      "io_stall_write", fun io -> fromInt64 (Units.Time Milliseconds) io.ioStallWriteMs 
      "io_stall", fun io -> fromInt64 (Units.Time Milliseconds) io.ioStall
      "num_of_reads", fun io -> fromInt64 (Units.Unit "reads") io.numOfReads
      "num_of_writes", fun io -> fromInt64 (Units.Unit "writes") io.numOfWrites
      "num_of_bytes_read", fun io -> fromInt64 Units.Bytes io.numOfBytesRead 
      "num_of_bytes_written", fun io -> fromInt64 Units.Bytes io.numOfBytesRead
      "read_latency", IOInfo.readLatency >> fromFloat (Units.Time Milliseconds)
      "write_latency", IOInfo.writeLatency >> fromFloat (Units.Time Milliseconds)
      "latency", IOInfo.latency >> fromFloat (Units.Time Milliseconds)
      "bytes_per_read", IOInfo.bytesPerRead >> fromFloat Units.Bytes
      "bytes_per_write", IOInfo.bytesPerWrite >> fromFloat Units.Bytes
      "bytes_per_transfer", IOInfo.bytesPerTransfer >> fromFloat Units.Bytes
    ]
    |> Map.ofList : Map<string, DPCalculator>

  let formatPrefixMetric prefix metric =
    sprintf "%s_%s" prefix metric

  let formatFull prefix metric instance =
    DP [ "logary"; "sql_server_health"; formatPrefixMetric prefix metric; instance ]

  let findCalculator metric =
    match datapoints |> Map.tryFind metric with
    | None -> failwithf "unknown metric: '%s'" metric
    | Some calculator -> calculator

  /// ns: logary.sql_server_health
  /// prefix: drive | file (metric name prefix)
  /// instance: intelliplan.mdf
  let dps prefix instance : DP list =
    datapoints
    |> Seq.map (fun kv -> kv.Key) |> Seq.toList
    |> List.map (fun metric -> formatFull prefix metric instance)

  let cleanDrive = removeColon
  let driveDps = removeColon >> dps "drive"
  let cleanFile = Path.GetFileName >> dotToUnderscore
  let fileDps = cleanFile >> dps "file"

  let byDrive drive info =
    cleanDrive info.drive = drive
  let byDbName db info =
    info.dbName = db
  let byFile file info =
    cleanFile info.filePath = file

  let loop (conf : Conf) (ri : RuntimeInfo) (inbox : IActor<_>) =
    let log = LogLine.setPath "Logary.Metrics.SQLServerHealth"
              >> Logger.log ri.logger

    let rec init () = async {
      LogLine.info "init" |> log
      let connMgr  = Sql.withNewConnection conf.openConn
      let measures = ioInfo connMgr |> Seq.cache
      let driveDps =
        measures
        |> Seq.distinctBy (fun m -> m.drive)
        |> Seq.map (fun l -> l.drive |> driveDps)
        |> List.ofSeq
        |> List.concat
      let fileDps =
        measures
        |> Seq.distinctBy (fun info -> info.filePath)
        |> Seq.map (fun info -> info.filePath |> fileDps)
        |> List.ofSeq
        |> List.concat
      return! running { connMgr     = connMgr
                        lastIOs     = List.ofSeq measures, Date.now ()
                        deltaIOs    = [], Duration.Epsilon
                        dps         = driveDps @ fileDps
                        // first measurement isn't delta based, can't be returned
                        sampleFresh = false }
      }

    and running
      // TODO: how to represent the duration when sending to the target?
      // should we divide to get a rate and send the rate?
      ({ deltaIOs = deltas, dur
         lastIOs  = lastIOMeasures, lastIOInstant  } as state) = async {
      let! msg, _ = inbox.Receive()
      match msg with
      | GetValue (_, replChan) when not state.sampleFresh ->
        replChan.Reply []
        return! running state

      | GetValue (dps, replChan) ->
        // TODO: use conf.latencyTargets
        let values =
          dps |> List.fold (fun s t ->
            match t with
            | DP [_ns; _probe; metricName; instance] ->
              LogLine.infof "get value, curr dp: %A" t |> log
              let calculator, summer =
                match metricName with
                | _ when metricName.StartsWith("drive_") ->
                  findCalculator (metricName.Substring("drive_".Length)),
                  byDrive instance
                | _ when metricName.StartsWith("database_") ->
                  findCalculator (metricName.Substring("database_".Length)),
                  byDbName instance
                | _ when metricName.StartsWith("file_") ->
                  findCalculator (metricName.Substring("file_".Length)),
                  byFile instance
                | _ -> failwithf "unknown metric name %s" metricName
              // TODO: summing ALL DPs for EVERY DP, instead of summing once and
              // reading from there
              calculator (IOInfo.sumBy deltas summer) t
            | dp -> failwithf "unknown %A" dp
            :: s) []
        replChan.Reply values
        return! running { state with sampleFresh = false }

      | GetDataPoints replChan ->
        LogLine.info "get dps" |> log
        replChan.Reply state.dps
        return! running state

      | Update msr ->
        LogLine.infof "does not support updating path %s" msr.m_path.joined |> log
        return! running state

      | Sample when state.sampleFresh ->
        return! running state

      | Sample ->
        LogLine.info "sample" |> log
        let now    = Date.now ()
        let curr   = ioInfo state.connMgr |> List.ofSeq
        let dio    = (lastIOMeasures, curr) ||> List.map2 delta
        let dioDur = now - lastIOInstant
        return! running { state with lastIOs     = curr, now
                                     deltaIOs    = dio, dioDur
                                     sampleFresh = true }

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

