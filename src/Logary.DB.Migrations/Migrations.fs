namespace Logary.DB.Migrations

/// Some names for schemas
module Defaults =
  [<Literal>]
  let Schema = "Logary"

  [<Literal>]
  let MetricsTable = "Metrics"

  [<Literal>]
  let LogLinesTable = "LogLines"

open System
open System.Reflection

open FluentMigrator
open FluentMigrator.VersionTableInfo
open FluentMigrator.Runner
open FluentMigrator.Runner.Processors
open FluentMigrator.Runner.Initialization
open FluentMigrator.Runner.Announcers

open Logary

/// https://github.com/schambers/fluentmigrator/wiki/Create-custom-metadata-for-the-VersionInfo-table
[<VersionTableMetaData>]
type VersionTable() =
  inherit DefaultVersionTableMetaData()
    override x.SchemaName = Defaults.Schema

/// https://stackoverflow.com/questions/7574417/is-it-possible-to-use-fluent-migrator-in-application-start
type MigrationOptions(preview, switches, timeout) =
  interface IMigrationProcessorOptions with
    member x.PreviewOnly = preview
    member x.ProviderSwitches = switches
    member x.Timeout = timeout

/// A class that allows you to run the migrations from code
/// See: https://stackoverflow.com/questions/7574417/is-it-possible-to-use-fluent-migrator-in-application-start
type Runner(fac      : MigrationProcessorFactory,
            connStr  : string,
            ?timeout : TimeSpan,
            ?showSql : bool,
            ?logger  : Logger) =

  let logger = defaultArg logger (Logging.getCurrentLogger())

  let timeout = defaultArg timeout (TimeSpan.FromSeconds(60.))
  let showSql = defaultArg showSql true

  let mkRunner () =
    let announcer = new TextWriterAnnouncer(Log.info logger, ShowSql = showSql)
    let assembly  = Assembly.GetExecutingAssembly()
    let ctx       = new RunnerContext(announcer, Namespace = "Logary.DB.Migrations")
    let opts      = MigrationOptions(false, "", int timeout.TotalSeconds)
    let processor = fac.Create(connStr, announcer, opts)
    new MigrationRunner(assembly, ctx, processor)

  /// Migrate up to the latest version available in the assembly.
  member x.MigrateUp () =
    let r = mkRunner ()
    r.MigrateUp(true)

  /// Migrate down to version 0 (zero)
  member x.MigrateDown () =
    let r = mkRunner ()
    r.MigrateDown(0L, true)

[<Migration(1L)>]
type MetricsTable() =
  inherit AutoReversingMigration()
  override x.Up () =

    base.Create.Table(Defaults.MetricsTable).InSchema(Defaults.Schema)
      .WithColumn("Path").AsString(255).NotNullable().WithDefaultValue(String.Empty)
        .WithColumnDescription("Where's the metric taken from; see graphite type paths")
      .WithColumn("EpochTicks").AsInt64()
        .WithColumnDescription("No. of ticks since Unix Epoch. 1 tick = 100 ns = 10 000 ms")
      .WithColumn("Level").AsInt16().NotNullable()
        .WithColumnDescription("See LogLevel.fs for disc union.")
      .WithColumn("Type").AsString(55).NotNullable()
        .WithColumnDescription("guauge|counter|timer")
      .WithColumn("Value").AsDouble().NotNullable()
        .WithColumnDescription("The value of the measure.")
      .WithColumn("CorrelationId").AsString(255).Nullable().WithDefaultValue(null)
        .WithColumnDescription("Arbitrary correlation id from context")
    |> ignore

    base.Create.Index("IX_Metrics_EpochTicksTypeLevel").OnTable(Defaults.MetricsTable)
      .OnColumn("EpochTicks").Descending().WithOptions().NonClustered()
      .OnColumn("Type").Ascending().WithOptions().NonClustered()
      .OnColumn("Level").Ascending().WithOptions().NonClustered()
    |> ignore

[<Migration(2L)>]
type LogLinesTable() =
  inherit AutoReversingMigration()
  override x.Up () =

    base.Create.Table(Defaults.LogLinesTable).InSchema(Defaults.Schema)
      .WithColumn("Message").AsString().NotNullable()
        .WithColumnDescription("The LogLine message")
      .WithColumn("Data").AsString().Nullable()
        .WithColumnDescription("Optional data")
      .WithColumn("Path").AsString(255).NotNullable().WithDefaultValue(String.Empty)
        .WithColumnDescription("Where's the metric taken from; see graphite type paths")
      .WithColumn("EpochTicks").AsInt64()
        .WithColumnDescription("No. of ticks since Unix Epoch. 1 tick = 100 ns = 10 000 ms")
      .WithColumn("Level").AsInt16().NotNullable()
        .WithColumnDescription("See LogLevel.fs for disc union.")
      .WithColumn("Exception").AsString().Nullable()
        .WithColumnDescription("Optional exception data, reflected from the instance")
      .WithColumn("Tags").AsString().Nullable()
        .WithColumnDescription("optional, comma-separated tags")
      .WithColumn("CorrelationId").AsString(255).Nullable().WithDefaultValue(null)
        .WithColumnDescription("Arbitrary correlation id from context")
    |> ignore

    base.Create.Index("IX_LogLines_EpochTicksLevelMessage").OnTable(Defaults.LogLinesTable).InSchema(Defaults.Schema)
      .OnColumn("EpochTicks").Descending().WithOptions().NonClustered()
      .OnColumn("Level").Ascending().WithOptions().NonClustered()
      .OnColumn("Message").Descending().WithOptions().NonClustered()
    |> ignore

