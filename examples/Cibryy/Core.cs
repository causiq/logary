using System;
using Cibryy.Logging;

namespace Cibryy
{
    // Note: this library has no reference to Logary proper

    public static class Core
    {
        static readonly ILogger _logger = Log.Create("Cibryy.Core");

        static Exception M4(string prefix)
        {
            throw new Exception($"{prefix} (a sample exception)");
        }

        static Exception M3(string prefix)
        {
            return M4(prefix);
        }

        static Exception M2(string prefix)
        {
            return M3(prefix);
        }

        static Exception M1(string prefix)
        {
            return M2(prefix);
        }

        public static Exception GetSampleException(string messagePrefix)
        {
            try
            {
                return M1(messagePrefix);
            }
            catch (Exception e)
            {
                return e;
            }
        }

        public static int Work(ILogger logger)
        {
            logger.LogWithAck(
                LogLevel.Warn,
                msg => msg.SetEvent("Hey {user}!")
                    .SetField("user", "haf")
                    .SetNameEnding("Work"))
                .Wait();
            return 42;
        }

        public static int WorkNonAsync(ILogger logger)
        {
            logger.Log(msg =>
                msg.SetEvent("Hey {user}!")
                    .SetLevel(LogLevel.Warn)
                    .SetField("user", "haf")
                    .SetName("Cibryy.Core.WorkNonAsync")
                    .SetTimestamp(1470047883029045000L));

            return 45;
        }

        public static void SimpleWork(ILogger logger)
        {
            logger.Log(msg => msg.SetLevel(LogLevel.Error).SetEvent("Too simplistic"));
        }

        public static int GenerateAndLogExn(ILogger logger)
        {
            var ex = GetSampleException("Ohoh!");
            logger.Log(msg => msg
                .SetLevel(LogLevel.Error)
                .SetEvent("An error with an attached exception")
                .AddException(ex)
                .AddException(new Exception("another")));
            return 59;
        }

        public static int StaticWork()
        {
            _logger.Debug(msg => msg.SetEvent("A debug log"));
            return 49;
        }
    }
}
