module Logary.Metrics.SQLServerHealth

open FSharp.Actor

open Logary
open Logary.Internals
open Logary.Metric
open Logary.AsmUtils

type Conf =
  { contentsOf : ResourceName -> string
    }

/// default config
let empty =
  { contentsOf = readResource }

module private Impl =

  type Drive  = string
  type DBFile = string

  type State =
    { lastLatencies : obj
      lastStalls    : obj
      lastPLE       : obj }

  // example data point, collected for a Duration

  // ns.category.perf_counter.instance
  // ns    .component        .metric_name  .instance.specific_dp_value.calculated
  // logary.sql_server_health.drive_latency.c       .read_ms
  // logary.sql_server_health.drive_latency.c       .read_ms          .mean
  // logary.sql_server_health.drive_latency.c       .read_ms          .max

  // guage shaped, polled ever n ms
  // logary.sql_server_health.drive_latency.c          .read
  // logary.sql_server_health.db_latency   .intelliplan.read

  let baseName = "logary.sql_server_health"
  let driveLatency = String.concat "." [baseName; "drive_latency"]
  let fileLatency = String.concat "." [baseName; "dbfile_latency"]

  // histogram shaped, values added every n ms
  // logary.sql_server_health.drive_latency.c.read_ms.mean
  // logary.sql_server_health.drive_latency.c.read_ms.max
  // logary.sql_server_health.drive_latency.c.read_ms.p99
  // logary.sql_server_health.drive_latency.c.read_ms.p75

  let loop (conf : Conf) (ri : RuntimeInfo) (inbox : IActor<_>) =
    let rec init () = async {
      return! running ()
      }
    
    and running state = async {
      let! msg, _ = inbox.Receive()
      match msg with
      | GetValue (dps, replChan) ->
        replChan.Reply []
        return! running ()

      | GetDataPoints replChan ->
        let res = [] // TODO: need to query to get baseline drives and files
          
        replChan.Reply res
        return! running ()

      | Update msr ->
        match msr.m_value'' with
        | Some v -> return! running ()
        | None   -> return! running ()

      | Sample ->
        return! running state

      | Shutdown ackChan ->
        ackChan.Reply Ack
        return! shutdown state

      | Reset ->
        return! reset state
      }

    and reset state = async {
      return! init ()
      }

    and shutdown state = async {
      return ()
      }

    init ()

/// Create the new SQLServer Health metric
let create conf = MetricUtils.stdNamedMetric Probe (Impl.loop conf)

/// C# interop: Create a new Noop metric that doesn't do very much
[<CompiledName "Create">]
let create' (conf, name) =
  create conf name

[<EntryPoint>]
let main argv =
  printfn "%s" (AsmUtils.readResource argv.[0])
  0 // return an integer exit code
