namespace Logary.DB.Migrations

open System
open System.Reflection

open FluentMigrator

[<Migration(1L)>]
type MetricsTable() =
  inherit AutoReversingMigration()

  override x.Up () =
    let ctx : string = x.ApplicationContext :?> string
    let indexForReading = ctx.Contains Runner.IndexForReading

    base.Create.Table(Defaults.MetricsTable).InSchema(Defaults.Schema)
      .WithColumn("Host").AsString(255).NotNullable()
        .WithColumnDescription("Hostname/DNS name of sender")
      .WithColumn("Path").AsString(255).NotNullable().WithDefaultValue(String.Empty)
        .WithColumnDescription("Where''s the metric taken from; see graphite type paths")
      .WithColumn("EpochTicks").AsInt64()
        .WithColumnDescription("No. of ticks since Unix Epoch. 1 tick = 100 ns = 10 000 ms")
      .WithColumn("Level").AsInt16().NotNullable()
        .WithColumnDescription("See LogLevel.fs for disc union.")
      .WithColumn("Value").AsDouble().NotNullable()
        .WithColumnDescription("The value of the measure.")
      .WithColumn("CorrelationId").AsString(255).Nullable().WithDefaultValue(null)
        .WithColumnDescription("Arbitrary correlation id from context")
    |> ignore

    // if you're just shipping
    base.Create.Index("IX_Metrics_EpochTicks").OnTable(Defaults.MetricsTable)
      .OnColumn("EpochTicks").Descending().WithOptions().NonClustered()
    |> ignore

    // if you're also reading
    if indexForReading then
      base.Create.Index("IX_Metrics_EpochTicks.Path.Type.Level").OnTable(Defaults.MetricsTable)
        .WithOptions().NonClustered()
        .OnColumn("EpochTicks").Descending()
        .OnColumn("Path").Ascending()
        .OnColumn("Level").Ascending()
      |> ignore

[<Migration(2L)>]
type LogLinesTable() =
  inherit AutoReversingMigration()

  override x.Up () =
    let ctx : string = x.ApplicationContext :?> string
    let indexForReading = ctx.Contains Runner.IndexForReading

    base.Create.Table(Defaults.LogLinesTable).InSchema(Defaults.Schema)
      .WithColumn("Host").AsString(255).NotNullable()
        .WithColumnDescription("Hostname/DNS name of sender")
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
    
    // if you're just storing and deleting:
    base.Create.Index("IX_LogLines_EpochTicks").OnTable(Defaults.MetricsTable)
      .OnColumn("EpochTicks").Descending().WithOptions().NonClustered()
    |> ignore

    // if you're also reading:
    if indexForReading then
      base.Create.Index("IX_LogLines_EpochTicks.Level.Message").OnTable(Defaults.LogLinesTable).InSchema(Defaults.Schema)
        .OnColumn("EpochTicks").Descending().WithOptions().NonClustered()
        .OnColumn("Level").Ascending().WithOptions().NonClustered()
        .OnColumn("Message").Ascending().WithOptions().NonClustered()
      |> ignore

