using System;
using System.Threading.Tasks;
using Logary.Configuration;

namespace Logary.ElmahIO.CSharpExample
{
    public class Program
    {
        static async Task Sample(Logger logger)
        {
            await logger.LogEvent(LogLevel.Info, "Hello world", new
            {
                important = "yes"
            }, backpressure: true);
            
            logger.LogEvent(LogLevel.Fatal, "Fatal application error on finaliser thread").Wait();

            await logger.LogEvent(LogLevel.Verbose, "immegawd immegawd immegawd!!", new
            {
                tags = new[] { "tag1", "tag2" }
            }, backpressure: true);

            var val = await logger.Time(async () =>
                {
                    for (int i = 0; i < 100; i++)
                        await Task.Delay(1);

                    return 32;
                }, "computeAnswerToEverything")();

            if (val != 32) throw new Exception($"Expected val=32, but was {val}");

            await logger.LogEventFormat(LogLevel.Warn, "{0} is the answer to the universe and everything", val);

            await logger.Time(
                    () => logger.LogEvent(LogLevel.Debug, "I wonder how long this takes", backpressure: true))
                ();

            try
            {
                throw new ApplicationException("thing went haywire");
            }
            catch (Exception e)
            {
                await logger.LogEventFormat(LogLevel.Fatal, "Unhandled {exception}!", e);
            }
        }

        public static int Main(string[] args)
        {
            var logId = Environment.GetEnvironmentVariable("ELMAH_IO_LOG_ID");
            if (logId == null) throw new InvalidOperationException("Missing key 'ELMAH_IO_LOG_ID' from environment");

            using (var logary = LogaryFactory.New("Logary.ElmahIO Sample",
                with => with
                    .Target<Targets.ElmahIO.Builder>(
                        "elmah.io",
                        conf => conf.Target.WithLogId(Guid.Parse(logId)))
                ).Result)
            {
                var logger = logary.GetLogger("Logary.ElmahIOSample");
                Sample(logger).Wait();
            }
            return 0;
        }
    }
}
