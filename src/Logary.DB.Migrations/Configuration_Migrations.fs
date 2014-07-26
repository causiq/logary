[<AutoOpen>]
module Logary.Configuration.Migrations

open System.Data

open FluentMigrator

open Logary
open Logary.DB
open Logary.Targets.DB
open Logary.DB.Migrations

/// Migrate the given DB Conf up to the latest version of the Logary.DB target
let migrateUp
  (conf : DBConf)
  (processorFac : IDbConnection -> IMigrationProcessor) =

  let conn = conf.connFac ()
  let fac = ExistingConnectionProcessorFactory(conn, processorFac)
  Runner(fac, "").MigrateUp()
