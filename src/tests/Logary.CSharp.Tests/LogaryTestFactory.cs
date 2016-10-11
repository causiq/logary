using System.IO;
using System.Text.RegularExpressions;
using Logary.Configuration;
using Logary.Targets;
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
                        new Microsoft.FSharp.Core.FSharpOption<Formatting.StringFormatter>(Formatting.StringFormatterModule.levelDatetimeMessagePathNl),
                        new Microsoft.FSharp.Core.FSharpOption<object>(new object())),
                    "tw");

            var twRule =
                RuleModule.Create(new Regex(@"^Logary\.CSharp\.Tests"), "tw", LogLevel.Verbose,
                    message => true);

            var internalTarg = Console.Create(Console.empty, "console");

            return Config.Configure(
                    "Logary.CSharp.Tests C# low level API",
                    new[] { twTarg },
                    new Metric.MetricConf[0],
                    new[] { twRule },
                    LogLevel.Warn,
                    internalTarg)
                .ToTask()
                .Result;
        }
    }
}