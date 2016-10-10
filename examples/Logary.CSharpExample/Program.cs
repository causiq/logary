using System;
using System.Text.RegularExpressions;
using Logary;
using Logary.Configuration;
using Logary.Targets;

/*
 * 
    
//   .Target<Logary.Targets.RabbitMQ.Builder>(
//       "rabbitmq",
//       conf => conf.Target.EnableTls("./cert/path.pfx", "TopSecret12345").Done()
//   )
// currently Windows only:
/*
    .Target<DB.Builder>("db",
        conf => conf.Target
            .ConnectionFactory(() => new SQLiteConnection())
            .DefaultSchema()
            .MigrateUp(
                conn => new SqliteProcessor(conn,
                    new SqliteGenerator(),
                    new ConsoleAnnouncer(),
                    new MigrationOptions(false, "", 60),
                                            new SqliteDbFactory())))
*/

namespace Logary.CSharpExample
{
    public static class Program
    {
        public static int Main(string[] args)
        {
            using (var logary = LogaryFactory.New("Logary.CSharpExample",
                with => with
                    //.WithInternalLevel(LogLevel.Verbose)
                    //.Metrics(m => m.WithAllCounters())
                    .Target<TextWriter.Builder>(
                    "console1",
                    conf =>
                    conf.Target.WriteTo(System.Console.Out, System.Console.Error)
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
                        "logstash",
                        conf => conf.Target
                            .PublishTo("logstash.service")
                            .LogMetrics()
                            .Done()
                    )
                    .Target<ElasticSearch.Builder>(
                        "es",
                        conf => conf.Target
                            .PublishTo("elasticsearch.service")
                            .Type("logs") // index-name
                            .Done()
                    )
                    .Target<InfluxDb.Builder>("influx",
                                              conf => conf.Target.DB("http://influxdb.service:8086").Done())
                ).Result)
            {
                var logger = logary.GetLogger("Logary.CSharpExample");

                logger.LogEvent(LogLevel.Debug, "Hello world. Important? {important}", new
                {
                    important = "yes"
                });

                logger.LogEvent(LogLevel.Fatal, "Fatal application error on finaliser thread");

                logger.LogEvent(LogLevel.Verbose, "immegawd immegawd immegawd!!", new
                {
                    tags = new[] { "tag1", "tag2" }
                });

                var message = MessageModule.Event(LogLevel.Warn, "Here be dragons!");
                logger.LogWithAck(message).Result.Wait();

                var val = logger.Time(() =>
                    {
                        for (int i = 0; i < 100; i++)
                            System.Threading.Thread.Sleep(1);

                        return 32;
                }, "sample.config.computeAnswerToEverything")();

                logger.LogEventFormat(LogLevel.Warn, "{0} is the answer to the universe and everything", val);

                logger.Time(() => logger.LogEvent(LogLevel.Debug, "I wonder how long this takes"));

                try
                {
                    throw new ApplicationException("thing went haywire");
                }
                catch (Exception e)
                {
                    logger.LogEventFormat(LogLevel.Fatal, "Unhandled {exception}!", e);
                }
            }
            return 0;
        }
    }
}

