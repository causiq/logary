using System.Text;
using System.Text.RegularExpressions;
using Logary;
using Logary.Configuration;
using Logary.Target;
using Machine.Specifications;
using NodaTime;

// ReSharper disable InconsistentNaming
// ReSharper disable UnusedMember.Local
// ReSharper disable FieldCanBeMadeReadOnly.Local
// ReSharper disable UnusedMember.Global

namespace Intelliplan.Logary.Specs
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
                             .SourceMatching(new Regex(".*"))));
            };

        Cleanup cleanup = () =>
            {
                manager.Dispose();
                writer.Dispose();
            };

        Because reason = () =>
            {
                manager.GetLogger("Intelliplan.Logary.Specs.When_configuring_with_CSharp_API")
                       .Warn("the situation is dire", "oh-noes");
                manager.FlushPending(Duration.FromSeconds(20L));
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
                manager = LogaryFactory.New("Logary Specs", with =>
                    with.Target<TextWriter.Builder>(
                        "sample string writer", t =>
                            t.Target.WriteTo(writer, writer)
                             .AcceptIf(line => !line.path.Contains("When_configuring_filter_with_API"))));
            };

        Cleanup cleanup = () =>
            {
                manager.Dispose();
                writer.Dispose();
            };

        Because reason = () =>
            {
                manager.GetLogger("Intelliplan.Logary.Specs.When_configuring_filter_with_API")
                       .Warn("the situation is dire", "oh-noes");
                manager.FlushPending(Duration.FromSeconds(20L));
                subject = writer.ToString();
            };

        It output_should_be_empty = () => subject.ShouldEqual("");

        static LogManager manager;
        static System.IO.StringWriter writer;
        static string subject;
    }
}
