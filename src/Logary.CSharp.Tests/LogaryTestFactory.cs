using System.IO;
using Logary.Configuration;
using TextWriter = Logary.Targets.TextWriter;

namespace Logary.CSharp.Tests
{
    public static class LogaryTestFactory
    {
        public static LogManager GetManager()
        {
            var tw = new StringWriter();
            return ConfigureForTextWriter(tw);
        }

        public static LogManager GetManager(out StringWriter allOutput)
        {
            allOutput = new StringWriter();
            return ConfigureForTextWriter(allOutput);
        }

        public static LogManager ConfigureForTextWriter(StringWriter tw)
        {
            var twTarg =
                TextWriter.Create(
                    TextWriter.TextWriterConf.Create(tw, tw,
                        new Microsoft.FSharp.Core.FSharpOption<MessageWriter>(MessageWriterModule.levelDatetimeMessagePathNewLine)),
                    "tw");

            var config = Config.create("Logary.CSharp.Tests C# low level API","localhost");
            config = Config.ilogger(ILogger.NewConsole(LogLevel.Warn),config);
            config = Config.target(twTarg, config);
            config = Config.loggerMinLevel(".*", LogLevel.Verbose, config);
            var logary = Config.build(config).ToTask().Result;
            return logary;
        }
    }
}