namespace Intelliplan.Logary.SqlServer.Migrations

open System

open FluentMigrator

[<Migration(1L)>]
type MetricsTable() =
  inherit AutoReversingMigration()
  override x.Up () =

    base.Create.Table("Metrics").InSchema("Logary")
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

    base.Create.Index("IX_Timestamp").OnTable("Metrics")
      .OnColumn("Timestamp").Descending().WithOptions().NonClustered()
      .OnColumn("Type").Ascending().WithOptions().NonClustered()
      .OnColumn("Level").Ascending().WithOptions().NonClustered()
    |> ignore

[<Migration(2L)>]
type LogLineTable() =
  inherit AutoReversingMigration()
  override x.Up () =

    base.Create.Table("LogLines").InSchema("Logary")
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

    base.Create.Index("IX_Timestamp").OnTable("Metrics")
      .OnColumn("Timestamp").Descending().WithOptions().NonClustered()
      .OnColumn("Type").Ascending().WithOptions().NonClustered()
      .OnColumn("Level").Ascending().WithOptions().NonClustered()
    |> ignore

