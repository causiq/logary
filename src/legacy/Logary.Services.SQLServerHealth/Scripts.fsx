#I "bin/Release/net461"
#r "FsSql"
#r "Logary"
#r "Hopac.Core"
#r "Hopac"
#r "NodaTime"
#r "System.Data"
#load "AsmUtils.fs"
#load "SQLServerPLE.fs"
#load "Metrics_SQLServerHealth.fs"

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

open Logary.Metrics.SQLServerPLE
open Logary.Metrics.SQLServerHealth

Database.ple connMgr
