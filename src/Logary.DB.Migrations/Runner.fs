namespace Logary.DB.Migrations

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

  let mkRunner appContext =
    let announcer = new TextWriterAnnouncer(Log.info logger, ShowSql = showSql)
    let assembly  = Assembly.GetExecutingAssembly()
    let ctx       = new RunnerContext(announcer,
                                      Namespace = "Logary.DB.Migrations",
                                      ApplicationContext = appContext)
    let opts      = MigrationOptions(false, "", int timeout.TotalSeconds)
    let processor = fac.Create(connStr, announcer, opts)
    new MigrationRunner(assembly, ctx, processor)

  /// Migrate up to the latest version available in the assembly.
  /// You can optionally pass a string with an ApplicationContext, see
  /// https://github.com/schambers/fluentmigrator/wiki/ApplicationContext%3A-Passing-parameters-to-Migrations#to-set-the-applicationcontext-when-running-migrateexe
  /// e.g. Runner( ... ).MigrateUp("indexForReading,futureOption")
  ///
  /// - indexForReading - a feature toggle to add index for reading from the
  ///                     LogLines and Metrics tables, however they will slow down writes
  ///                     and make it much harder to continuously ship the data from the table
  ///                     Strongly typed from Runner.IndexForReading.
  member x.MigrateUp ?appContext =
    let appContext = defaultArg appContext ""
    let r = mkRunner appContext
    r.MigrateUp(true)

  /// Migrate down to version 0 (zero)
  member x.MigrateDown ?appContext =
    let appContext = defaultArg appContext ""
    let r = mkRunner appContext
    r.MigrateDown(0L, true)

  /// the string that is the 'indexForReading' option
  static member IndexForReading = "indexForReading"

open System.Data

/// used to avoid closing the SQLite connection in between migrations
type ExistingConnectionProcessorFactory(conn : IDbConnection, processorFac : IDbConnection -> IMigrationProcessor) =
  inherit MigrationProcessorFactory()
  override x.Create (connStr : string, accouncer : IAnnouncer, opts : IMigrationProcessorOptions) =
    processorFac conn
