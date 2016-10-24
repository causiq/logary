module Integration

open Expecto
open System
open System.Data
open System.Data.SqlClient
open FsSql
open Logary.Metrics.SQLServerHealth

// a function that opens a connection
let openConn() =
  let conn = new SqlConnection("Data Source=.;Integrated Security=SSPI;Database=master;")
  conn.Open()
  conn :> IDbConnection

let connMgr =
  Sql.withNewConnection openConn

[<Tests>]
let integration =
  testList "[integration] executing sql statements" [
    testCase "reading io info" <| fun _ ->
      Tests.skiptest "No SQL Server on OS X"
      let calculated =
        Database.ioInfo connMgr
        |> List.ofSeq
        |> List.map IOInfo.readLatency
        |> List.sort

      Expect.isFalse (calculated.Length = 0) "should not be empty"
    ]
