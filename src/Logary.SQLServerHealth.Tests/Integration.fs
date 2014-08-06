module Integration

open Fuchu

open System
open System.Data
open System.Data.SqlClient

open FsSql

open Logary.Metrics.SQLServerIOInfo

// a function that opens a connection
let openConn() =
  let conn = new SqlConnection("Data Source=.;Integrated Security=SSPI;Database=master;")
  conn.Open()
  conn :> IDbConnection

let connMgr = Sql.withNewConnection openConn

[<Tests>]
let integration =
  testList "[integration] executing sql statements" [
    testCase "reading io info" <| fun _ ->
      let calculated =
        Database.ioInfo connMgr
        |> List.ofSeq |> List.map IOInfo.readLatency |> List.sort
      Assert.Equal("should not be empty", calculated.Length = 0, false)
    ]
