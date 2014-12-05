using System;
using System.Diagnostics;
using System.Diagnostics.Contracts;
using System.IO;
using System.Runtime.CompilerServices;
using System.Text.RegularExpressions;
using Logary.Configuration;
using Machine.Specifications;
using NodaTime;
using Console = Logary.Targets.Console;
using TextWriter = Logary.Targets.TextWriter;

// ReSharper disable InconsistentNaming
// ReSharper disable UnusedMember.Local
// ReSharper disable FieldCanBeMadeReadOnly.Local
// ReSharper disable UnusedMember.Global

namespace Logary.Specs
{
    static class LogaryTestFactory
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
            var twTarg = TextWriter.Create(Formatting.StringFormatter.LevelDatetimeMessagePathNl,
                                           tw, tw, false, LogLevel.Error, "tw");
            var twRule = RuleModule.Create(new Regex(@"^Intelliplan\.Logary\.Specs"),
                                           "tw", l => true, m => true, LogLevel.Verbose);

            var internalTarg = Console.Create("cons", Console.empty);

            return Config.Configure(
                "Logary Specs C# low level API",
                new[] {twTarg},
                Duration.FromSeconds(4L),
                new Metric.MetricConf[0],
                new[] {twRule},
                LogLevel.Verbose, internalTarg);
        }
    }

    public class When_setting_up_logary
    {
        Because setting_up_logging = () =>
            {
                manager = LogaryTestFactory.GetManager();
                logger = manager.GetLogger("Intelliplan.Logary.Specs");
            };

        Cleanup afterwards = () => manager.Dispose();

        static LogManager manager;
        static Logger logger;

        It can_log_verbose = () => logger.Verbose("Hello world");
        It can_log_debug = () => logger.Debug("Hello world");
        It can_log_info = () => logger.Info("Hello world");
        It can_log_warn = () => logger.Warn("Hello world");
        It can_log_error = () => logger.Error("Hello world");
        It can_log_fatal = () => logger.Fatal("Hello world");
    }

    [Pure]
    public class When_getting_current_logger_name
    {
        static string subject = Logging.GetCurrentLoggerName();
        static string nlogName = GetCurrentClassLogger();

        It should_have_name_of_class_and_namespace = () => subject.ShouldEqual("Intelliplan.Logary.Specs.When_getting_current_logger_name");
        It should_have_the_same_name_as_the_NLog_algorithm = () => nlogName.ShouldEqual(subject);

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
        Establish context_setting_up_logging = () => manager = LogaryTestFactory.GetManager(out output);
        
        Because logging_line_and_flushing = () =>
            {
                subject.Info("logged line");

                manager.FlushPending(Duration.FromSeconds(20L));
            };

        It should_write_messages_to_text_writer = () => output.ToString().ShouldContain("logged line");

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

                thrownException = Catch.Exception(() => subject.Info("logged line"));
                flushThrown = Catch.Exception(() => manager.FlushPending(Duration.FromSeconds(20L)));
            };

        Cleanup afterwards = () => manager.Dispose();

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
                logger.Debug("da 1st line", "testing");
                manager.FlushPending(Duration.FromSeconds(20L));
                var written = output.ToString();
                written.ShouldContain("da 1st line");
                written.ShouldContain("testing");
                manager.Dispose();
            };

        Because initialising_again = () =>
            {
                manager = LogaryTestFactory.GetManager(out output);
                var logger = GetLogger();
                logger.Debug("2nd here we go", "testing");
                manager.FlushPending(Duration.FromSeconds(20L));
                subject = output.ToString();
            };

        It should_successfully_have_logged_string =
            () => subject.ShouldContain("2nd here we go");

        It shold_successfully_have_logged_tag =
            () => subject.ShouldContain("testing");

        Cleanup cleanup = () => manager.Dispose();

        static LogManager manager;
        static StringWriter output;
        static string subject;

        static Logger GetLogger()
        {
            return manager.GetLogger("Intelliplan.Logary.Specs.When_initialising_then_disposing_then_reinitialising");
        }
    }
}
