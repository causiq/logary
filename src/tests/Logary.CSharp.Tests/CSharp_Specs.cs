// ReSharper disable InconsistentNaming
// ReSharper disable UnusedMember.Local
// ReSharper disable FieldCanBeMadeReadOnly.Local
// ReSharper disable UnusedMember.Global

using System;
using System.IO;
using System.Text;
using System.Text.RegularExpressions;
using Logary.Configuration;
using Machine.Specifications;
using NodaTime;
using TextWriter = Logary.Targets.TextWriter;

namespace Logary.CSharp.Tests
{
    public class When_configuring_with_CSharp_API
    {
        static LogManager manager;
        static StringWriter writer;
        static string subject;
        static Instant timestamp;
        static Exception exception;

        Establish context = () =>
            {
                writer = new StringWriter(new StringBuilder());
                timestamp = Instant.FromUnixTimeSeconds(987654);
                exception = new ApplicationException("Nice exception");

                manager = LogaryFactory.New(
                    "Logary.CSharp.Tests",
                    with =>
                        with.Use(Middleware.ProcessName)
                            .Target<TextWriter.Builder>("sample string writer",
                            t => t.Target.WriteTo(writer, writer)
                                  .MinLevel(LogLevel.Verbose)
                                  .SourceMatching(new Regex(".*")))).Result;
            };

        Because reason = () =>
            {
                var logger = manager.GetLogger("Logary.CSharp.Tests.When_configuring_with_CSharp_API");
                logger.LogEvent(LogLevel.Warn, "the situation is dire, says {@foo}",
                        new {foo = "oh-noes"}, exception,
                        msg => msg.SetTimestamp(timestamp),
                        true,
                        true)
                    .Wait();
                subject = writer.ToString();
            };

        It output_should_contain_exception = () => subject.ShouldContain(exception.Message);
        It output_should_contain_message = () => subject.ShouldContain("the situation is dire");
        It output_should_contain_the_field = () => subject.ShouldContain("oh-noes");
        It output_should_contain_timestamp = () => subject.ShouldContain(timestamp.ToUnixTimeTicks().ToString());
        It output_should_contain_processName_key = () => subject.ShouldContain("processName");

        Cleanup cleanup = () =>
            {
                manager.Dispose();
                writer.Dispose();
            };
    }

    public class When_configuring_with_CSharp_API_and_using_setter_transformer
    {
        static LogManager manager;
        static StringWriter writer;
        static string subject;
        static Instant timestamp;
        static Exception exception;

        Establish context = () =>
            {
                writer = new StringWriter(new StringBuilder());
                timestamp = Instant.FromUnixTimeTicks(987654);
                exception = new ApplicationException("Nice exception");

                manager = LogaryFactory.New(
                    "Logary.CSharp.Tests",
                    with =>
                        with.Target<TextWriter.Builder>(
                            "sample string writer",
                            t =>
                                t.Target.WriteTo(writer, writer)
                                    .MinLevel(LogLevel.Verbose)
                                    .SourceMatching(new Regex(".*")))).Result;
            };

        Because reason = () =>
            {
                var logger = manager.GetLogger("Logary.CSharp.Tests.When_configuring_with_CSharp_API_and_using_setter_transformer");

                logger.LogEvent(
                    LogLevel.Warn,
                    "the situation is dire",
                    transform:
                    msg => msg
                        .SetFieldsFromObject(new {foo = "oh-noes"})
                        .SetTimestamp(timestamp)
                        .AddException(exception)
                        .SetContextFromObject(new {contextdata = "the Contextdata"}),
                    backpressure:true
                ).Wait();

                manager.FlushPending(Duration.FromSeconds(8L)).Wait();
                subject = writer.ToString();
            };

        It output_should_contain_context = () => subject.ShouldContain("the Contextdata");
        It output_should_contain_exception = () => subject.ShouldContain(exception.Message);

        It output_should_contain_message = () => subject.ShouldContain("the situation is dire");
        It output_should_contain_the_field = () => subject.ShouldContain("oh-noes");
        It output_should_contain_timestamp = () => subject.ShouldContain(timestamp.ToUnixTimeTicks().ToString());

        Cleanup cleanup = () =>
            {
                manager.Dispose();
                writer.Dispose();
            };
    }

    public class When_configuring_filter_with_API
    {
        static LogManager manager;
        static StringWriter writer;
        static string subject;

        Establish context = () =>
            {
                writer = new StringWriter(new StringBuilder());
                manager = LogaryFactory.New("Logary.CSharp.Tests",
                        with => with.Target<TextWriter.Builder>(
                            "sample string writer",
                            t => t.Target.WriteTo(writer, writer)
                                .AcceptIf(line => !line.name.ToString().Contains("When_configuring_filter_with_API"))))
                    .Result;
            };

        Because reason = () =>
            {
                manager
                    .GetLogger("Logary.CSharp.Tests.When_configuring_filter_with_API")
                    .LogEvent(LogLevel.Warn, "the situation is dire", new {error = "oh-noes"}, flush:true)
                    .Wait();
                subject = writer.ToString();
            };

        It output_should_be_empty = () => subject.ShouldEqual("");

        Cleanup cleanup = () =>
            {
                manager.Dispose();
                writer.Dispose();
            };
    }
}