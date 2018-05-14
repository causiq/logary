using System;
using System.IO;
using Logary.Configuration;
using Machine.Specifications;
using NodaTime;

// ReSharper disable InconsistentNaming
// ReSharper disable UnusedMember.Local
// ReSharper disable FieldCanBeMadeReadOnly.Local
// ReSharper disable UnusedMember.Global

namespace Logary.CSharp.Tests
{
    public class When_setting_up_logary
    {
        Because setting_up_logging = () =>
            {
                manager = LogaryTestFactory.GetManager();
                logger = manager.GetLogger("Logary.CSharp.Tests");
            };

        Cleanup afterwards = () =>
            {
                // manager.Dispose();
            };

        static LogManager manager;
        static Logger logger;

        It can_log_verbose = () => logger.LogEvent(LogLevel.Verbose, "Hello world", backpressure:true).Wait();
        It can_log_debug = () => logger.LogEvent(LogLevel.Debug, "Hello world", backpressure: true).Wait();
        It can_log_info = () => logger.LogEvent(LogLevel.Info, "Hello world", backpressure: true).Wait();
        It can_log_warn = () => logger.LogEvent(LogLevel.Warn, "Hello world", backpressure: true).Wait();
        It can_log_error = () => logger.LogEvent(LogLevel.Error, "Hello world", backpressure: true).Wait();
        It can_log_fatal = () => logger.LogEvent(LogLevel.Fatal, "Hello world", backpressure: true).Wait();
        It can_log_verbose2 = () => logger.LogEvent(LogLevel.Verbose, "Hello world 2", flush: true).Wait();
        It can_log_debug2 = () => logger.LogEvent(LogLevel.Debug, "Hello world 2", flush: true).Wait();
        It can_log_info2 = () => logger.LogEvent(LogLevel.Info, "Hello world 2", flush: true).Wait();
        It can_log_warn2 = () => logger.LogEvent(LogLevel.Warn, "Hello world 2", flush: true).Wait();
        It can_log_error2 = () => logger.LogEvent(LogLevel.Error, "Hello world 2", flush: true).Wait();
        It can_log_fatal2 = () => logger.LogEvent(LogLevel.Fatal, "Hello world 2", flush: true).Wait();
    }



    public class When_logging_with_logger_gotten_from_GetCurrentLogger
    {
        Establish context_setting_up_logging =
            () => manager = LogaryTestFactory.GetManager(out output);

        Because logging_line_and_flushing = () =>
            subject.LogEvent(LogLevel.Info, "logged line", flush: true).Wait();

        It should_write_messages_to_text_writer =
            () => output.ToString().ShouldContain("logged line");

        Cleanup afterwards = () =>
            {
                Hopac.Hopac.run(manager.shutdown());
            };

        static Logger subject = Log.Create<When_logging_with_logger_gotten_from_GetCurrentLogger>();
        static LogManager manager;
        static StringWriter output;
    }

    public class When_logging_after_LogManager_Dispose
    {
        Establish context_setting_up_logging = () => manager = LogaryTestFactory.GetManager(out output);

        Because logging_line_and_flushing = () =>
            {
                manager.shutdown().ToTask().Wait();

                thrownException = Catch.Exception(() => subject.LogEvent(LogLevel.Info, "logged line", backpressure: true).Wait());
                flushThrown = Catch.Exception(() => manager.FlushPending(Duration.FromSeconds(8L)).Wait());
            };

        Cleanup afterwards = () =>
        {
            // manager.DisposeAsync().ToTask().Wait();
        } ;

        It should_not_throw_when_writing_to_Logger_instance = () => thrownException.ShouldBeNull();
        It should_not_throw_when_calling_FlushPending = () => flushThrown.ShouldBeNull();
        It should_not_write_messages_to_text_writer = () => output.ToString().ShouldBeEmpty();

        static Logger subject = Log.Create<When_logging_after_LogManager_Dispose>();
        static LogManager manager;
        static StringWriter output;
        static Exception thrownException;
        static Exception flushThrown;
    }

    public class When_initialising_then_disposing_then_reinitialising
    {
        Establish context_setting_up_logging = () =>
            {
                manager = LogaryTestFactory.GetManager(out output);
                var logger = GetLogger();
                logger.LogEvent(
                        LogLevel.Info,
                        "da 1st line",
                        new {tags = new[] {"testing"}},
                        backpressure: true)
                    .Wait();

                manager.FlushPending(Duration.FromSeconds(20L)).Wait();

                var written = output.ToString();
                written.ShouldContain("da 1st line");
                written.ShouldContain("testing");
                // manager.Dispose();
            };

        Because initialising_again = () =>
            {
                manager = LogaryTestFactory.GetManager(out output);
                var logger = GetLogger();
                logger.LogEvent(
                    LogLevel.Debug, "2nd here we go",
                    new {fields = new[] {"2nd testing"}},
                    flush: true).Wait();
                subject = output.ToString();
            };

        It should_successfully_have_logged_string =
            () => subject.ShouldContain("2nd here we go");

        It should_successfully_have_logged_tag =
            () => subject.ShouldContain("2nd testing");

        Cleanup cleanup = () => {
            // manager.Dispose();
        };

        static LogManager manager;
        static StringWriter output;
        static string subject;
        static Logger GetLogger()
        {
            return manager.GetLogger("Logary.CSharp.Tests.When_initialising_then_disposing_then_reinitialising");
        }
    }
}
