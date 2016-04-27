using System.Text;
using System.Text.RegularExpressions;
using Logary.Configuration;
using Logary.Targets;
using Machine.Specifications;
using NodaTime;

// ReSharper disable InconsistentNaming
// ReSharper disable UnusedMember.Local
// ReSharper disable FieldCanBeMadeReadOnly.Local
// ReSharper disable UnusedMember.Global

namespace Logary.Specs
{
    public class When_configuring_with_CSharp_API
    {
        Establish context = () =>
            {
                writer = new System.IO.StringWriter(new StringBuilder());
                manager = LogaryFactory.New("Logary Specs", with =>
                    with.Target<TextWriter.Builder>(
                        "sample string writer", t =>
                            t.Target.WriteTo(writer, writer)
                             .MinLevel(LogLevel.Verbose)
                             .SourceMatching(new Regex(".*"))))
                    .Result;
            };

        Cleanup cleanup = () =>
            {
                manager.Dispose();
                writer.Dispose();
            };

        Because reason = () =>
            {
                manager.GetLogger("Logary.Specs.When_configuring_with_CSharp_API")
                       .LogEvent(LogLevel.Warn, "the situation is dire", "oh-noes");
                manager.FlushPending(Duration.FromSeconds(8L));
                subject = writer.ToString();
            };

        It output_should_contain_message = () => subject.ShouldContain("the situation is dire");
        It output_should_contain_the_tag = () => subject.ShouldContain("oh-noes");

        static LogManager manager;
        static System.IO.StringWriter writer;
        static string subject;
    }

    public class When_configuring_filter_with_API
    {
        Establish context = () =>
            {
                writer = new System.IO.StringWriter(new StringBuilder());
                manager = LogaryFactory.New("Logary Specs",
                    with => with.Target<TextWriter.Builder>(
                        "sample string writer",
                        t => t.Target.WriteTo(writer, writer)
                              .AcceptIf(line => !line.name.ToString().Contains("When_configuring_filter_with_API"))))
                    .Result;
            };

        Cleanup cleanup = () =>
            {
                manager.Dispose();
                writer.Dispose();
            };

        Because reason = () =>
            {
                manager.GetLogger("Logary.Specs.When_configuring_filter_with_API")
                    .LogEvent(LogLevel.Warn, "the situation is dire", "oh-noes");
                manager.FlushPending(Duration.FromSeconds(8L));
                subject = writer.ToString();
            };

        It output_should_be_empty = () => subject.ShouldEqual("");

        static LogManager manager;
        static System.IO.StringWriter writer;
        static string subject;
    }
}
