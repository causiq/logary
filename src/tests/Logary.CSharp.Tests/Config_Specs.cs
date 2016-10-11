using System;
using System.Diagnostics;
using System.Diagnostics.Contracts;
using System.IO;
using System.Runtime.CompilerServices;
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

        Cleanup afterwards = () => manager.Dispose();

        static LogManager manager;
        static Logger logger;

        It can_log_verbose = () => logger.LogEvent(LogLevel.Verbose, "Hello world").Wait();
        It can_log_debug = () => logger.LogEvent(LogLevel.Debug, "Hello world").Wait();
        It can_log_info = () => logger.LogEvent(LogLevel.Info, "Hello world").Wait();
        It can_log_warn = () => logger.LogEvent(LogLevel.Warn, "Hello world").Wait();
        It can_log_error = () => logger.LogEvent(LogLevel.Error, "Hello world").Wait();
        It can_log_fatal = () => logger.LogEvent(LogLevel.Fatal, "Hello world").Wait();
    }

    [Pure, Ignore("Does not work on Mono")]
    public class When_getting_current_logger_name
    {
        static PointName subject = Logging.GetCurrentLoggerName();
        static string nlogName = GetCurrentClassLogger();

        It should_have_name_of_class_and_namespace = () => subject.ShouldEqual(PointNameModule.Parse("Logary.CSharp.Tests.When_getting_current_logger_name"));
        It should_have_the_same_name_as_the_NLog_algorithm = () => nlogName.ShouldEqual(PointNameModule.Format(subject));

        [MethodImpl(MethodImplOptions.NoInlining)]
        public static string GetCurrentClassLogger()
        {
            string loggerName;
            Type declaringType;
            int framesToSkip = 1;
            do
            {
                var frame = new StackFrame(framesToSkip, false);
                var method = frame.GetMethod();
                declaringType = method.DeclaringType;
                if (declaringType == null)
                {
                    loggerName = method.Name;
                    break;
                }

                framesToSkip++;
                loggerName = declaringType.FullName;
            } while (declaringType.Module.Name.Equals("mscorlib.dll", StringComparison.OrdinalIgnoreCase));

            return loggerName;
        }
    }

    public class When_logging_with_logger_gotten_from_GetCurrentLogger
    {
        Establish context_setting_up_logging =
            () => manager = LogaryTestFactory.GetManager(out output);

        Because logging_line_and_flushing = () =>
            {
                subject.LogEvent(LogLevel.Info, "logged line").Wait();
                manager.FlushPending(Duration.FromSeconds(20L)).Wait();
            };

        It should_write_messages_to_text_writer =
            () => output.ToString().ShouldContain("logged line");

        Cleanup afterwards = () => manager.Dispose();

        static Logger subject = Logging.GetCurrentLogger();
        static LogManager manager;
        static StringWriter output;
    }

    public class When_logging_after_LogManager_Dispose
    {
        Establish context_setting_up_logging = () => manager = LogaryTestFactory.GetManager(out output);

        Because logging_line_and_flushing = () =>
            {
                manager.Dispose();

                thrownException = Catch.Exception(() => subject.LogEvent(LogLevel.Info, "logged line").Wait());
                flushThrown = Catch.Exception(() => manager.FlushPending(Duration.FromSeconds(20L)).Wait());
            };

        //Cleanup afterwards = () => manager.Dispose();

        It should_not_throw_when_writing_to_Logger_instance = () => thrownException.ShouldBeNull();
        It should_not_throw_when_calling_FlushPending = () => flushThrown.ShouldBeNull();
        It should_not_write_messages_to_text_writer = () => output.ToString().ShouldBeEmpty();

        static Logger subject = Logging.GetCurrentLogger();
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
                logger.LogEvent(LogLevel.Info, "da 1st line", new { tags = new[] { "testing" } }).Wait();

                manager.FlushPending(Duration.FromSeconds(20L)).Wait();

                var written = output.ToString();
                written.ShouldContain("da 1st line");
                written.ShouldContain("testing");
                manager.Dispose();
            };

        Because initialising_again = () =>
            {
                manager = LogaryTestFactory.GetManager(out output);
                var logger = GetLogger();
                logger.LogEvent(
                    LogLevel.Debug, "2nd here we go",
                    new { fields = new[] { "2nd testing" } }).Wait();
                manager.FlushPending(Duration.FromSeconds(20L)).Wait();
                subject = output.ToString();
            };

        It should_successfully_have_logged_string =
            () => subject.ShouldContain("2nd here we go");

        It should_successfully_have_logged_tag =
            () => subject.ShouldContain("2nd testing");

        Cleanup cleanup = () => manager.Dispose();

        static LogManager manager;
        static StringWriter output;
        static string subject;
        static Logger GetLogger()
        {
            return manager.GetLogger("Logary.CSharp.Tests.When_initialising_then_disposing_then_reinitialising");
        }
    }
}
