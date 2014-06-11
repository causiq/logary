namespace Logary.Configuration

open FluentMigrator

open System
open System.Data
open System.Runtime.CompilerServices

open Logary
open Logary.Targets
open Logary.DB
open Logary.DB.Migrations
open Logary.Target.DB

[<Extension>]
module MigrationBuilderEx =
  let private logger = Logging.getCurrentLogger ()

  [<Extension; CompiledName "MigrateUp">]
  let migrateUp (builder      : ThirdStep,
                 processorFac : Func<IDbConnection, IMigrationProcessor>) =
    let builder' = builder :> FactoryApi.ConfigReader<DBConf>
    let conf = builder'.ReadConf()
    let conn = conf.connFac ()
    let fac = ExistingConnectionProcessorFactory(conn, fun conn -> processorFac.Invoke conn)
    Runner(fac, "", logger = logger).MigrateUp()
    builder.Done conf