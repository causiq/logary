using System;
using System.Data.SQLite;
using System.Text.RegularExpressions;
using FluentMigrator.Runner.Announcers;
using FluentMigrator.Runner.Generators.SQLite;
using FluentMigrator.Runner.Processors.SQLite;
using Logary;
using Logary.Configuration;
//using Logary.DB.Migrations;
using Logary.Targets;
using Console = System.Console;

namespace Logary.Specs.Examples
{
    public class When_using_fluent_API
    {
        public void UsageExample()
        {
            var x = LogaryFactory.New("Logary Specs",
                with => with.Target<TextWriter.Builder>(
                    "console1",
                    conf =>
                        conf.Target.WriteTo(Console.Out, Console.Error)
                            .MinLevel(LogLevel.Verbose)
                            .AcceptIf(line => true)
                            .SourceMatching(new Regex(".*"))
                    )
                    .Target<Graphite.Builder>(
                        "graphite",
                        conf => conf.Target.ConnectTo("127.0.0.1", 2131)
                    )
                    .Target<Debugger.Builder>("debugger")
                    .Target<Logstash.Builder>(
                        "ls",
                        conf => conf.Target
                            .PublishTo("tcp://192.168.100.99:5001")
                            .Done())
                    //.Target<Logary.Targets.DB.Builder>("db",
                    //    conf => conf.Target
                    //       .ConnectionFactory(() => new SQLiteConnection())
                    //        .DefaultSchema()
                    //        .MigrateUp(
                    //            conn => new SqliteProcessor(conn,
                    //                new SqliteGenerator(),
                    //                new ConsoleAnnouncer(),
                    //                new MigrationOptions(false, "", 60),
                    //                new SqliteDbFactory())))
                ).Result;

            var logger = x.GetLogger("Sample.Config");

            logger.LogEvent(LogLevel.Debug, "Hello world",  new
                {
                    important = "yes"
                });

            logger.LogEvent(LogLevel.Fatal, "Fatal application error on finaliser thread");

            logger.LogEvent(LogLevel.Verbose, "immegawd immegawd immegawd!!", new {
                tags = new [] { "tag1", "tag2" }
            });

            var val = logger.Time(() =>
                {
                    for (int i = 0; i < 100; i++)
                        System.Threading.Thread.Sleep(1);

                    return 32;
                }, nameEnding: "compute_answer_to_everything");

            logger.LogEventFormat(LogLevel.Warn, "{theAnswer} is the answer to the universe and everything", val);

            logger.Time(() => logger.LogEvent(LogLevel.Debug, "I wonder how long this takes", new {
                tags = new[] { "introspection", "navel-gazing" }
            }));

            try
            {
                throw new ApplicationException("thing went haywire");
            }
            catch (Exception e)
            {
                logger.LogEvent(LogLevel.Fatal, "expecting haywire, so we're telling with debug", new {
                    tags = new[] {  "haywire", "external" },
                }, exn: e);
            }
        }
    }
}
