namespace Logary.DB.Migrations

open System
open System.Reflection

open FluentMigrator

module Literals =
  [<Literal>]
  let LogLevelDesc =
    "Verbose | Debug | Info | Warn | Error | Fatal"

  [<Literal>]
  let EpochNanosSesc =
    "No. of nanoseconds since Unix Epoch. 1 tick = 100 ns = 10 000 ms"

module SharedBetweenTables =

  let messageStructure (x: Builders.Create.Table.ICreateTableWithColumnOrSchemaOrDescriptionSyntax) =
    x
      .WithColumn("Host").AsString(255).NotNullable()
        .WithColumnDescription("Hostname/DNS name of sender")

      .WithColumn("Name").AsString(255).NotNullable().WithDefaultValue(String.Empty)
        .WithColumnDescription("Where's the event sourced")

      // JSON
      .WithColumn("Fields").AsString().Nullable()
        .WithColumnDescription("Any fields relevant to the template message")

      // JSON
      .WithColumn("Context").AsString().Nullable()
        .WithColumnDescription("Any context relevant to the template message")

      .WithColumn("EpochNanos").AsInt64()
        .WithColumnDescription(Literals.EpochNanosSesc)

      .WithColumn("Level").AsInt16().NotNullable()
        .WithColumnDescription(Literals.LogLevelDesc)

[<Migration(1L)>]
type GaugesTable() =
  inherit AutoReversingMigration()

  override x.Up () =
    let ctx: string = x.ApplicationContext :?> string
    let indexForReading = ctx.Contains Runner.IndexForReading

    base.Create
      .Table(Defaults.GaugesTable).InSchema(Defaults.Schema)

      .WithColumn("Value").AsDouble().NotNullable()
        .WithColumnDescription("The value of the measure.")

      |> SharedBetweenTables.messageStructure

    |> ignore

    // if you're just shipping
    base.Create.Index("IX_Gauges_EpochNanos").OnTable(Defaults.GaugesTable).InSchema(Defaults.Schema)
      .OnColumn("EpochNanos").Descending().WithOptions().NonClustered()
    |> ignore

    // if you're also reading
    if indexForReading then
      base.Create.Index("IX_Gauges_EpochNanos.Name.Level").OnTable(Defaults.GaugesTable).InSchema(Defaults.Schema)
        .WithOptions().NonClustered()
        .OnColumn("EpochNanos").Descending()
        .OnColumn("Name").Ascending()
        .OnColumn("Level").Ascending()
      |> ignore

[<Migration(2L)>]
type EventsTable() =
  inherit AutoReversingMigration()

  override x.Up () =
    let ctx: string = x.ApplicationContext :?> string
    let indexForReading = ctx.Contains Runner.IndexForReading

    base.Create
      .Table(Defaults.EventsTable).InSchema(Defaults.Schema)

      .WithColumn("Template").AsString().NotNullable()
        .WithColumnDescription("The template of the logged event")

      |> SharedBetweenTables.messageStructure

    |> ignore
    
    // if you're just storing and deleting:
    base.Create.Index("IX_Events_EpochNanos").OnTable(Defaults.EventsTable).InSchema(Defaults.Schema)
      .OnColumn("EpochNanos").Descending().WithOptions().NonClustered()
    |> ignore

    // if you're also reading:
    if indexForReading then
      base.Create.Index("IX_Events_EpochNanos.Level.Template").OnTable(Defaults.EventsTable).InSchema(Defaults.Schema)
        .OnColumn("EpochNanos").Descending().WithOptions().NonClustered()
        .OnColumn("Level").Ascending().WithOptions().NonClustered()
        .OnColumn("Template").Ascending().WithOptions().NonClustered()
      |> ignore