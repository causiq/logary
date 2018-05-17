// ReSharper disable InconsistentNaming
// ReSharper disable UnusedMember.Local
// ReSharper disable FieldCanBeMadeReadOnly.Local
// ReSharper disable UnusedMember.Global

using System.IO;
using System.Text;
using System.Threading;
using Logary.Configuration;
using Machine.Specifications;
using TextWriter = Logary.Targets.TextWriter;

namespace Logary.CSharp.Tests
{
    public class When_using_TimeWithAck
    {
        static LogManager manager;
        static StringWriter writer;
        static string subject;

        Establish context = () =>
        {
            writer = new StringWriter(new StringBuilder());
            manager = LogaryFactory.New(
                "Logary.CSharp.Tests", "localhost",
                with =>
                    with.InternalLogger(ILogger.NewConsole(LogLevel.Fatal))
                        .LoggerMinLevel(".*", LogLevel.Verbose)
                        .Target<TextWriter.Builder>("sample string writer",
                            t => t.Target.WriteTo(writer, writer))).Result;
        };

        Because reason = () =>
        {
            var logger = manager.GetLogger("Timers");

            // Action
            logger.Time(() => { }, "Action")();

            // Func<>
            var func1 = logger.Time(() => 32, "Func<>")();
            func1.ShouldEqual(32);

            // Func<,>
            var func2 = logger.Time<int, int>(i => i, "Func<,>")(10);
            func2.ShouldEqual(10);

            using (logger.TimeScope("TimeScope"))
                Thread.Sleep(0);

            subject = writer.ToString();
        };

        It output_should_contain_Action = () => subject.ShouldContain("Action");
        It output_should_contain_Func1 = () => subject.ShouldContain("Func<>");
        It output_should_contain_Func2 = () => subject.ShouldContain("Func<,>");
        It output_should_contain_gauge = () => subject.ShouldContain("gauge");
        It output_should_not_contain_TimeScope = () => subject.ShouldNotContain("TimeScope");

        Cleanup cleanup = () =>
        {
            // manager.Dispose();
            writer.Dispose();
        };
    }
}