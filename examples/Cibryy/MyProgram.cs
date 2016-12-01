using Logary.Facade;

namespace Cibryy
{
    public class MyProgram
    {
        static readonly ILogger _logger = Log.Create();

        public static int Work (ILogger logger)
        {
            logger.InfoWithBP(msg => msg.SetEvent("Started")).Wait();
            return 42;
        }

        public static int StaticWork()
        {
            _logger.Debug(msg => msg.SetEvent("A debug log"));
            return 49;
        }
    }
}
