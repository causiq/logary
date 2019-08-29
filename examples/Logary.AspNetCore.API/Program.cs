using System.Threading.Tasks;
using Logary.Configuration;
using Logary.Targets;
using Logary.Trace.Sampling;
using Microsoft.AspNetCore;
using Microsoft.AspNetCore.Hosting;
using Microsoft.Extensions.Logging;
using ILogger = Logary.Configuration.ILogger;

namespace Logary.AspNetCore.API
{
    public class Program
    {
        public static async Task Main(string[] args)
        {
            var logary = await LogaryFactory.New("Logary.AspNetCore.API", config =>
                config
                    .InternalLogger(ILogger.NewLiterateConsole(LogLevel.Verbose))
                    .LoggerMinLevel(".*", LogLevel.Verbose)
                    .Target<LiterateConsole.Builder>(
                        "literate",
                        lit => lit.Target.WithSingleLineTokeniser().Done())
                    .Target<Logary.Targets.Jaeger.Builder>(
                        "jaeger",
                        x =>
                            x.Target
                                .WithJaegerAgent("localhost", 30831)
                                .WithSampler(new PerKeySampler(0.2, 100))
                                .Done())
            );

            CreateWebHostBuilder(args, logary).Build().Run();
        }

        static IWebHostBuilder CreateWebHostBuilder(string[] args, LogManager logary)
        {
            return WebHost.CreateDefaultBuilder(args)
                .ConfigureServices(services => services.AddLogary(logary))
                .ConfigureLogging(logging => logging.AddLogary(logary))
                .UseStartup<Startup>();
        }
    }
}