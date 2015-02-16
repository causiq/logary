#I "bin/Release"

#r "Fuchu.dll"
open Fuchu

#r "Logary.dll"
#r "Logary.DB.dll"
#r "Logary.DB.Migrations.dll"

#r "FluentMigrator.dll"
#r "FluentMigrator.Runner.dll"

#r "FsSql.dll"

#r "FSharp.Actor.dll"

#r "System.Data.dll"
#r "System.Data.SQLite.dll"

#load "DBTarget.fs"
open Logary.DB.Tests

targetTests
|> Test.filter (fun s -> s.Contains "log and read back returns result")
|> run