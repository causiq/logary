namespace Logary.CSharpExample
{
    using System;
    using System.Threading;
    using System.Threading.Tasks;
    using Configuration;
    using CSharp;
    using Targets;
    using Adapters.Facade;

    public static class Program
    {
        public static Task<LogManager> StartLiterate()
        {
            return LogaryFactory.New("Logary.CSharpExample","laptop",
                with => with.InternalLogger(ILogger.NewConsole(LogLevel.Debug))
                        .Target<LiterateConsole.Builder>("console1"));
        }

        public static async Task SampleUsage(Logger logger)
        {
            // without async
            logger.LogSimple(MessageModule.Event(LogLevel.Info, "User logged in"));

            // await placing the Hello World event in the buffer
            await logger.LogEvent(LogLevel.Debug, "Hello world. Important? {important}", new
            {
                important = "yes"
            });

            // await logging the fatal event and getting an ack back from each of the configured
            // targets
            await logger.LogEvent(LogLevel.Fatal, "Fatal application error on finaliser thread.", waitForAck: true);

            await logger.LogEvent(LogLevel.Verbose, "We need to log this with backpressure.", new
            {
                tags = new[] { "tag1", "tag2" }
            });

            // alternatively, you can use the ack-explicit functions together with the
            // data object model that's MessageModule.
            var message = MessageModule.Event(LogLevel.Warn, "Here be dragons!");
            await logger.LogWithAck(message);

            var val = logger.Time(() =>
                    {
                        for (int i = 0; i < 100; i++)
                            Thread.Sleep(1);

                        return 32;
                    }, "sample.config.computeAnswerToEverything")
                ();

            await logger.LogEventFormat(LogLevel.Warn,
                "{horses} is the answer to the universe and everything",
                val);

            await logger.Time(
                    () => logger.LogEvent(LogLevel.Debug, "I wonder how long this takes"))
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

        public static void SampleCibryyUsage(Cibryy.Logging.ILogger logger)
        {
           Cibryy.Core.Work(logger);
           Cibryy.Core.WorkBackpressure(logger);
           Cibryy.Core.ErrorWithBP(logger);
           Cibryy.Core.SimpleWork(logger);
           Cibryy.Core.GenerateAndLogExn(logger);
           Cibryy.Core.StaticWork();
        }

        public static int Main(string[] args)
        {
            // normal console app boilerplate;
            var mre = new ManualResetEventSlim(false);
            System.Console.CancelKeyPress += (sender, arg) => mre.Set();

            var logary = StartLiterate().Result;

            // Usage with a library:
            LogaryFacadeAdapter.Initialise<Cibryy.Logging.ILogger>(logary);
            var logger = logary.GetLogger("main");
            SampleCibryyUsage(LoggerCSharpAdapter.Create<Cibryy.Logging.ILogger>(logger));

            // Usage in this program:
            SampleUsage(logger).Wait();

            // Wait for CTRL+C
            mre.Wait();
            return 0;
        }
    }
}
