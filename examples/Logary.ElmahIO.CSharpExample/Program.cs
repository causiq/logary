using System;
using Logary.Configuration;

namespace Logary.ElmahIO.CSharpExample
{
    public class Program
    {
        public static int Main(string[] args)
        {
            using (var logary = LogaryFactory.New("Logary.CSharpExample",
                with => with
                    .Target<Targets.ElmahIO.Builder>(
                        "elmah.io",
                        conf => conf.Target.WithLogId(Guid.Parse(Environment.GetEnvironmentVariable("ELMAH_IO_LOG_ID"))))
                ).Result)
            {
                var logger = logary.GetLogger("Logary.CSharpExample");

                logger.LogEvent(LogLevel.Info, "Hello world", new
                {
                    important = "yes"
                });

                logger.LogEvent(LogLevel.Fatal, "Fatal application error on finaliser thread");

                logger.LogEvent(LogLevel.Verbose, "immegawd immegawd immegawd!!", new
                {
                    tags = new[] { "tag1", "tag2" }
                });

                var val = logger.TimePath("sample.config.computeAnswerToEverything", () =>
                {
                    for (int i = 0; i < 100; i++)
                        System.Threading.Thread.Sleep(1);

                    return 32;
                });

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
