using System;
using System.Text.RegularExpressions;
using System.Threading;
using System.Threading.Tasks;
using Logary.Configuration;
using Logary.Targets;
using Console = System.Console;

namespace Logary.CSharp.Tests.Examples
{
    public class When_using_fluent_API
    {
        public async Task UsageExample()
        {
            var x = await LogaryFactory.New("Logary.CSharp.Tests.Examples","localhost",
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
                    .Target<LiterateConsole.Builder>("literate")
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
                );

            var logger = x.GetLogger("Sample.Config");

            await logger.LogEvent(LogLevel.Debug, "Hello world",  new
                {
                    important = "yes"
                });

            await logger.LogEvent(LogLevel.Fatal, "Fatal application error on finaliser thread");

            await logger.LogEvent(LogLevel.Verbose, "immegawd immegawd immegawd!!", new {
                tags = new [] { "tag1", "tag2" }
            });

            using (logger.TimeScope("TimeScope"))
                Thread.Sleep(0);

            var val = logger.Time(() =>
                    {
                        for (int i = 0; i < 100; i++)
                            Thread.Sleep(1);

                        return 32;
                    }, "compute_answer_to_everything")
                ();

            await logger.LogEventFormat(LogLevel.Warn, "{theAnswer} is the answer to the universe and everything", val);

            await logger.Time(() => logger.LogEvent(LogLevel.Debug, "I wonder how long this takes", new
                    {
                        tags = new[] {"introspection", "navel-gazing"}
                    }))
                ();

            try
            {
                throw new ApplicationException("thing went haywire");
            }
            catch (Exception e)
            {
                await logger.LogEvent(LogLevel.Fatal, "expecting haywire, so we're telling with debug", new {
                    tags = new[] {  "haywire", "external" },
                }, exn: e);
            }
        }
    }
}
