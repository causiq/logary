#I "bin/Release"
#r "FsSql.dll"
#r "Logary.dll"
#r "FSharp.Actor-logary.dll"
#r "System.Data.dll"
#load "AsmUtils.fs"
#load "SQLServerHealth.fs"

open System
open System.Data
open System.Data.SqlClient

open FsSql

// a function that opens a connection
let openConn() =
  let conn = new SqlConnection("Data Source=.;Integrated Security=SSPI;Database=master;")
  conn.Open()
  conn :> IDbConnection

// the connection manager, encapsulates how to create and dispose the connection
let connMgr = Sql.withNewConnection openConn

open Logary.Metrics.SQLServerHealth

Database.ple connMgr
Database.latencyInfo connMgr |> List.ofSeq |> List.map LatencyInfo.readLatency |> List.sort
