using System;
using System.Collections.Generic;
using System.Linq;
using Logary.Internals;
using NodaTime;

namespace Logary
{
    /// <summary>
    /// Extensions that make it nicer to use Logary with C#
    /// </summary>
    public static class LoggerExtensions
    {
        /// <summary>
        /// C#-oriented-method: Write a log line to the logger. Exposes all parameters in a way that should make
        /// it very easy to use the logging behaviour from C#.
        /// </summary>
        /// <remarks>
        /// This method takes the **LogLevel** first, and then the message; to avoid having to grapple with
        /// overload resolution. The other Log(string, LogLine ...) method is in F# and won't add the same
        /// good defaults as this method.
        /// </remarks>
        /// <param name="logger">Instance to invoke the extension method on</param>
        /// <param name="level"></param>
        /// <param name="message">A message to attach to the log line</param>
        /// <param name="data">Data to attach to the log line being sent to the target;
        /// e.g. if using LogStash, these properties will be fields. For performance
        /// improvements, you can send a dictionary, or otherwise you can
        /// send an anonymous object whose public properties are then serialised
        /// as a dictionary.
        /// This is the message that you want to log.
        /// It's worth noting that a message without string.Format-ed parameters, is more equal
        /// across time, and if you have custom data you want to pass, you should rather set
        /// that data on the 'data' property of LogLine (with SetData and SetDatas).
        /// </param>
        /// <param name="tags">A list of tags to attach to the log line</param>
        /// <param name="exception"></param>
        /// <param name="path"></param>
        /// <param name="timestamp">
        /// When the log entry was given; optional, defaults to when this method
        /// is called</param>
        public static void Log(
            this Logger logger,
            LogLevel level,
            string message,
            object data = null,
            IEnumerable<string> tags = null,
            Exception exception = null,
            string path = null,
            Instant? timestamp = null)
        {
            if (logger == null) throw new ArgumentNullException("logger");
            if (level == null) throw new ArgumentNullException("level");
            if (message == null) throw new ArgumentNullException("message");

            logger.Log(message, level, data, tags, path ?? logger.Name, exception,
                       timestamp ?? Date.now());
        }

        /// <summary>
        /// C#-oriented-method: Write a log line to the logger. Exposes all parameters in a way that should make
        /// it very easy to use the logging behaviour from C#.
        /// </summary>
        /// <remarks>
        /// This method takes the **LogLevel** first, and then the message; to avoid having to grapple with
        /// overload resolution. The other Log(string, LogLine ...) method is in F# and won't add the same
        /// good defaults as this method.
        /// </remarks>
        /// <param name="logger"></param>
        /// <param name="level"></param>
        /// <param name="message">This is the message that you want to log.
        /// It's worth noting that a message without string.Format-ed parameters, is more equal
        /// across time, and if you have custom data you want to pass, you should rather set
        /// that data on the 'data' property of LogLine (with SetData and SetDatas).</param>
        /// <param name="setterTransformer">
        /// There are extension methods in this library that go towards creating new instances
        /// of LogLine that you can use to change the value inside this function.
        /// </param>
        public static void Log(
            this Logger logger,
            LogLevel level,
            string message,
            Func<LogLine, LogLine> setterTransformer)
        {
            if (logger == null) throw new ArgumentNullException("logger");
            if (level == null) throw new ArgumentNullException("level");
            if (message == null) throw new ArgumentNullException("message");
            if (setterTransformer == null) throw new ArgumentNullException("setterTransformer");

            var line = LogLineModule.Create(level, message);
            logger.Log(setterTransformer(line));
        }

        /// <summary>
        /// TBD
        /// </summary>
        public static T Time<T>(this Logger logger, Func<T> f, LogLevel level = null)
        {
            if (logger == null) throw new ArgumentNullException("logger");
            if (f == null) throw new ArgumentNullException("f");
            level = level ?? LogLevel.Debug;
            return f();
        }

        /// <summary>
        /// TBD
        /// </summary>
        public static void Time(this Logger logger, Action a, LogLevel level = null)
        {
            if (logger == null) throw new ArgumentNullException("logger");
            if (a == null) throw new ArgumentNullException("a");
            level = level ?? LogLevel.Debug;
            a();
        }

        /// <summary>
        /// TBD
        /// </summary>
        public static T TimePath<T>(this Logger logger, string path, Func<T> f, LogLevel level = null)
        {
            if (logger == null) throw new ArgumentNullException("logger");
            if (path == null) throw new ArgumentNullException("path");
            if (f == null) throw new ArgumentNullException("f");
            return f();
        }

        /// <summary>
        /// Log a message with the string passed, in <see cref="string.Format(string,object)"/>.
        /// </summary>
        /// <param name="logger">logger to invoke the Log call on</param>
        /// <param name="level">The level that the log format is at</param>
        /// <param name="formatStringMessage">Message to log, may contain C#-esque format placeholders.</param>
        /// <param name="args">Arguments to the format string</param>
        public static void LogFormat(this Logger logger, LogLevel level, string formatStringMessage, params object[] args)
        {
            logger.Log<object>(string.Format(formatStringMessage, args), level, null);
        }

        /// <summary>
        /// Log a fatal log message with a collection optional of tags
        /// </summary>
        /// <param name="logger">logger to invoke the Log call on</param>
        /// <param name="message">Message to pass to the targets</param>
        /// <param name="tags">An optional collection of tags to attach to the log line</param>
        public static void Fatal(this Logger logger, string message, params string[] tags)
        {
            if (logger == null) throw new ArgumentNullException("logger");
            if (message == null) throw new ArgumentNullException("message");
            logger.Log<object>(message, LogLevel.Fatal, null, tags, logger.Name,
                               null, null);
        }

        /// <summary>
        /// Log a fatal log message with a collection optional of tags
        /// </summary>
        /// <param name="logger">logger to invoke the Log call on</param>
        /// <param name="message">Message to pass to the targets</param>
        /// <param name="e">The exception that occurred</param>
        /// <param name="tags">
        /// [Optional] Some tags - optional - if you pass them,
        /// you will also be tagging with <see cref="LogLineModule.ExceptionTag"/>.
        /// </param>
        public static void FatalException(this Logger logger, string message,
                                          Exception e, params string[] tags)
        {
            if (logger == null) throw new ArgumentNullException("logger");
            if (message == null) throw new ArgumentNullException("message");
            if (e == null) throw new ArgumentNullException("e");

            logger.Log<object>(message, LogLevel.Fatal, null,
                               MakeTags(LogLineModule.ExceptionTag, tags),
                               logger.Name, e, null);
        }

        /// <summary>
        /// Log a error log message with a collection optional of tags
        /// </summary>
        /// <param name="logger">logger to invoke the Log call on</param>
        /// <param name="message">Message to pass to the targets</param>
        /// <param name="tags">An optional collection of tags to attach to the log line</param>
        public static void Error(this Logger logger, string message, params string[] tags)
        {
            if (logger == null) throw new ArgumentNullException("logger");
            if (message == null) throw new ArgumentNullException("message");

            logger.Log<object>(message, LogLevel.Error, null, tags, logger.Name,
                               null, null);
        }

        /// <summary>
        /// Log a error log message with a collection optional of tags
        /// </summary>
        /// <param name="logger">[NotNull] logger to invoke the Log call on</param>
        /// <param name="message">[NotNull] Message to pass to the targets</param>
        /// <param name="e">[NotNull] The exception that occurred</param>
        /// <param name="tags">
        /// [Optional] Some tags - optional - if you pass them,
        /// you will also be tagging with <see cref="LogLineModule.ExceptionTag"/>.
        /// </param>
        public static void ErrorException(this Logger logger, string message, Exception e,
                                          params string[] tags)
        {
            if (logger == null) throw new ArgumentNullException("logger");
            if (message == null) throw new ArgumentNullException("message");
            if (e == null) throw new ArgumentNullException("e");
            logger.Log<object>(message, LogLevel.Error, null,
                               MakeTags(LogLineModule.ExceptionTag, tags),
                               logger.Name, e, null);
        }

        /// <summary>
        /// Log a warn log message with a collection optional of tags
        /// </summary>
        /// <param name="logger">logger to invoke the Log call on</param>
        /// <param name="message">Message to pass to the targets</param>
        /// <param name="tags">An optional collection of tags to attach to the log line</param>
        public static void Warn(this Logger logger, string message, params string[] tags)
        {
            logger.Log<object>(message, LogLevel.Warn, null, tags, logger.Name,
                               null, null);
        }

        /// <summary>
        /// Log a warn log message with a collection optional of tags
        /// </summary>
        /// <param name="logger">logger to invoke the Log call on</param>
        /// <param name="message">Message to pass to the targets</param>
        /// <param name="e">The exception that occurred</param>
        /// <param name="tags">
        /// [Optional] Some tags - optional - if you pass them,
        /// you will also be tagging with <see cref="LogLineModule.ExceptionTag"/>.
        /// </param>
        public static void WarnException(this Logger logger, string message, Exception e, params string[] tags)
        {
            if (logger == null) throw new ArgumentNullException("logger");
            if (message == null) throw new ArgumentNullException("message");
            if (e == null) throw new ArgumentNullException("e");
            logger.Log<object>(message, LogLevel.Warn, null,
                               MakeTags(LogLineModule.ExceptionTag, tags),
                               logger.Name, e, null);
        }

        /// <summary>
        /// Log a info log message with a collection optional of tags
        /// </summary>
        /// <param name="logger">logger to invoke the Log call on</param>
        /// <param name="message">Message to pass to the targets</param>
        /// <param name="tags">An optional collection of tags to attach to the log line</param>
        public static void Info(this Logger logger, string message, params string[] tags)
        {
            logger.Log<object>(message, LogLevel.Info, null, tags, logger.Name, null, null);
        }

        /// <summary>
        /// Log a info log message with a collection optional of tags
        /// </summary>
        /// <param name="logger">logger to invoke the Log call on</param>
        /// <param name="message">Message to pass to the targets</param>
        /// <param name="e">The exception that occurred</param>
        /// <param name="tags">
        /// [Optional] Some tags - optional - if you pass them,
        /// you will also be tagging with <see cref="LogLineModule.ExceptionTag"/>.
        /// </param>
        public static void InfoException(this Logger logger, string message, Exception e, params string[] tags)
        {
            if (logger == null) throw new ArgumentNullException("logger");
            if (message == null) throw new ArgumentNullException("message");
            if (e == null) throw new ArgumentNullException("e");

            logger.Log<object>(message, LogLevel.Info, null,
                               MakeTags(LogLineModule.ExceptionTag, tags),
                               logger.Name, e, null);
        }

        /// <summary>
        /// Log a debug log message with a collection optional of tags
        /// </summary>
        /// <param name="logger">logger to invoke the Log call on</param>
        /// <param name="message">Message to pass to the targets</param>
        /// <param name="tags">An optional collection of tags to attach to the log line</param>
        public static void Debug(this Logger logger, string message, params string[] tags)
        {
            logger.Log<object>(message, LogLevel.Debug, null, tags, logger.Name,
                               null, null);
        }

        /// <summary>
        /// Log a debug log message with a collection optional of tags
        /// </summary>
        /// <param name="logger">logger to invoke the Log call on</param>
        /// <param name="message">Message to pass to the targets</param>
        /// <param name="e">The exception that occurred</param>
        /// <param name="tags">
        /// [Optional] Some tags - optional - if you pass them,
        /// you will also be tagging with <see cref="LogLineModule.ExceptionTag"/>.
        /// </param>
        public static void DebugException(this Logger logger, string message, Exception e, params string[] tags)
        {
            if (logger == null) throw new ArgumentNullException("logger");
            if (message == null) throw new ArgumentNullException("message");
            if (e == null) throw new ArgumentNullException("e");

            logger.Log<object>(message, LogLevel.Debug, null,
                               MakeTags(LogLineModule.ExceptionTag, tags),
                               logger.Name, e, null);
        }

        /// <summary>
        /// Log a verbose log message with a collection optional of tags
        /// </summary>
        /// <param name="logger">logger to invoke the Log call on</param>
        /// <param name="message">Message to pass to the targets</param>
        /// <param name="tags">An optional collection of tags to attach to the log line</param>
        public static void Verbose(this Logger logger, string message, params string[] tags)
        {
            logger.Log<object>(message, LogLevel.Verbose, null, tags,
                               logger.Name, null, null);
        }

        /// <summary>
        /// Log a verbose log message with a collection optional of tags
        /// </summary>
        /// <param name="logger">logger to invoke the Log call on</param>
        /// <param name="message">Message to pass to the targets</param>
        /// <param name="e">The exception that occurred</param>
        /// <param name="tags">
        /// [Optional] Some tags - optional - if you pass them,
        /// you will also be tagging with <see cref="LogLineModule.ExceptionTag"/>.
        /// </param>
        public static void VerboseException(this Logger logger, string message, Exception e, params string[] tags)
        {
            if (logger == null) throw new ArgumentNullException("logger");
            if (message == null) throw new ArgumentNullException("message");
            if (e == null) throw new ArgumentNullException("e");
            logger.Log<object>(message, LogLevel.Verbose, null,
                               MakeTags(LogLineModule.ExceptionTag, tags),
                               logger.Name, e, null);
        }

        static IEnumerable<string> MakeTags(string logaryTag, params string[] tags)
        {
            if (tags == null || tags.Length == 0) return new[] {logaryTag};
            return tags.Union(new[] {logaryTag});
        }
    }
}
