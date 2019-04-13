using System.Text.RegularExpressions;
using System.Threading.Tasks;
using Logary.Configuration;
using Logary.Targets;

namespace Logary.CSharpExample
{
    public class StartEverything
    {
        public static Task<LogManager> Run()
        {
            return LogaryFactory.New("Logary.CSharpExample","localhost",
                with => with.InternalLogger(ILogger.NewConsole(LogLevel.Debug))
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
                            .Target<Elasticsearch.Builder>(
                                "es",
                                conf => conf.Target
                                    .PublishTo("elasticsearch.service")
                                    .Type("logs") // index-name
                                    .Done()
                            )
                            .Target<InfluxDb.Builder>("influx", conf => conf.Target.DB("http://influxdb.service:8086").Done())
                );
        }

    }
}