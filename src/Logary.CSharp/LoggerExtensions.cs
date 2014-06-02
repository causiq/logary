using System;
using System.Collections.Generic;
using System.Linq;

namespace Logary
{
    /// <summary>
    /// Extensions that make it nicer to use Logary with C#
    /// </summary>
    public static class LoggerExtensions
    {
        /// <summary>
        /// Write a log line to the logger. Exposes all parameters in a way that should make
        /// it very easy to use the logging behaviour from C#.
        /// </summary>
        /// <param name="logger">Instance to invoke the extension method on</param>
        /// <param name="level"></param>
        /// <param name="message">A message to attach to the log line</param>
        /// <param name="data">Data to attach to the log line being sent to the target;
        /// e.g. if using LogStash, these properties will be fields. For performance
        /// improvements, you can send a dictionary, or otherwise you can
        /// send an anonymous object whose public properties are then serialised
        /// as a dictionary.</param>
        /// <param name="tags">A list of tags to attach to the log line</param>
        /// <param name="exception"></param>
        /// <param name="path"></param>
        public static void Log(
            this Logger logger,
            LogLevel level,
            string message,
            object data = null,
            IEnumerable<string> tags = null,
            Exception exception = null,
            string path = null)
        {
            if (logger == null) throw new ArgumentNullException("logger");
            if (level == null) throw new ArgumentNullException("level");
            if (message == null) throw new ArgumentNullException("message");
            logger.Log(message, level, data, tags, path ?? logger.Name, exception);
        }

        /// <summary>
        /// Time the execution of the function passed and log the <see cref="MetricType"/>.Timer metric.
        /// </summary>
        /// <returns>f()</returns>
        public static T Time<T>(this Logger logger, Func<T> f, LogLevel level = null)
        {
            if (logger == null) throw new ArgumentNullException("logger");
            if (f == null) throw new ArgumentNullException("f");
            level = level ?? LogLevel.Debug;
            return Logary.Log.TimePath(logger, level, null, f);
        }

        /// <summary>
        /// Time the execution of the function passed and log the <see cref="MetricType"/>.Timer metric.
        /// </summary>
        public static void Time(this Logger logger, Action a, LogLevel level = null)
        {
            if (logger == null) throw new ArgumentNullException("logger");
            if (a == null) throw new ArgumentNullException("a");
            level = level ?? LogLevel.Debug;
            Logary.Log.TimePath(logger, level, null, () =>
                {
                    a();
                    return 0;
                });
        }

        /// <summary>
        /// Time the execution of the function passed and log the <see cref="MetricType"/>.Timer metric.
        /// This method allows you to specify the path that consistutes the 'key' in a graphing system,
        /// such as graphite.
        /// </summary>
        public static T TimePath<T>(this Logger logger, string path, Func<T> f, LogLevel level = null)
        {
            if (logger == null) throw new ArgumentNullException("logger");
            if (path == null) throw new ArgumentNullException("path");
            if (f == null) throw new ArgumentNullException("f");
            level = level ?? LogLevel.Debug;
            return Logary.Log.TimePath(logger, level, path, f);
        }

        /// <summary>
        /// Log a message with the string passed, in <see cref="string.Format(string,object)"/>.
        /// </summary>
        /// <param name="logger">Logger to invoke the Log call on</param>
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
        /// <param name="logger">Logger to invoke the Log call on</param>
        /// <param name="message">Message to pass to the targets</param>
        /// <param name="tags">An optional collection of tags to attach to the log line</param>
        public static void Fatal(this Logger logger, string message, params string[] tags)
        {
            if (logger == null) throw new ArgumentNullException("logger");
            if (message == null) throw new ArgumentNullException("message");
            logger.Log<object>(message, LogLevel.Fatal, null, tags, logger.Name, null);
        }

        /// <summary>
        /// Log a fatal log message with a collection optional of tags
        /// </summary>
        /// <param name="logger">Logger to invoke the Log call on</param>
        /// <param name="message">Message to pass to the targets</param>
        /// <param name="e">The exception that occurred</param>
        /// <param name="tags">
        /// [Optional] Some tags - optional - if you pass them,
        /// you will also be tagging with <see cref="Logary.Log.ExceptionTag"/>.
        /// </param>
        public static void FatalException(this Logger logger, string message, Exception e, params string[] tags)
        {
            if (logger == null) throw new ArgumentNullException("logger");
            if (message == null) throw new ArgumentNullException("message");
            if (e == null) throw new ArgumentNullException("e");
            logger.Log<object>(message, LogLevel.Fatal, null, MakeTags(Logary.Log.ExceptionTag, tags), logger.Name, e);
        }

        /// <summary>
        /// Log a error log message with a collection optional of tags
        /// </summary>
        /// <param name="logger">Logger to invoke the Log call on</param>
        /// <param name="message">Message to pass to the targets</param>
        /// <param name="tags">An optional collection of tags to attach to the log line</param>
        public static void Error(this Logger logger, string message, params string[] tags)
        {
            if (logger == null) throw new ArgumentNullException("logger");
            if (message == null) throw new ArgumentNullException("message");
            logger.Log<object>(message, LogLevel.Error, null, tags, logger.Name, null);
        }

        /// <summary>
        /// Log a error log message with a collection optional of tags
        /// </summary>
        /// <param name="logger">[NotNull] Logger to invoke the Log call on</param>
        /// <param name="message">[NotNull] Message to pass to the targets</param>
        /// <param name="e">[NotNull] The exception that occurred</param>
        /// <param name="tags">
        /// [Optional] Some tags - optional - if you pass them,
        /// you will also be tagging with <see cref="Logary.Log.ExceptionTag"/>.
        /// </param>
        public static void ErrorException(this Logger logger, string message, Exception e, params string[] tags)
        {
            if (logger == null) throw new ArgumentNullException("logger");
            if (message == null) throw new ArgumentNullException("message");
            if (e == null) throw new ArgumentNullException("e");
            logger.Log<object>(message, LogLevel.Error, null, MakeTags(Logary.Log.ExceptionTag, tags), logger.Name, e);
        }

        /// <summary>
        /// Log a warn log message with a collection optional of tags
        /// </summary>
        /// <param name="logger">Logger to invoke the Log call on</param>
        /// <param name="message">Message to pass to the targets</param>
        /// <param name="tags">An optional collection of tags to attach to the log line</param>
        public static void Warn(this Logger logger, string message, params string[] tags)
        {
            logger.Log<object>(message, LogLevel.Warn, null, tags, logger.Name, null);
        }

        /// <summary>
        /// Log a warn log message with a collection optional of tags
        /// </summary>
        /// <param name="logger">Logger to invoke the Log call on</param>
        /// <param name="message">Message to pass to the targets</param>
        /// <param name="e">The exception that occurred</param>
        /// <param name="tags">
        /// [Optional] Some tags - optional - if you pass them,
        /// you will also be tagging with <see cref="Logary.Log.ExceptionTag"/>.
        /// </param>
        public static void WarnException(this Logger logger, string message, Exception e, params string[] tags)
        {
            if (logger == null) throw new ArgumentNullException("logger");
            if (message == null) throw new ArgumentNullException("message");
            if (e == null) throw new ArgumentNullException("e");
            logger.Log<object>(message, LogLevel.Warn, null, MakeTags(Logary.Log.ExceptionTag, tags), logger.Name, e);
        }

        /// <summary>
        /// Log a info log message with a collection optional of tags
        /// </summary>
        /// <param name="logger">Logger to invoke the Log call on</param>
        /// <param name="message">Message to pass to the targets</param>
        /// <param name="tags">An optional collection of tags to attach to the log line</param>
        public static void Info(this Logger logger, string message, params string[] tags)
        {
            logger.Log<object>(message, LogLevel.Info, null, tags, logger.Name, null);
        }

        /// <summary>
        /// Log a info log message with a collection optional of tags
        /// </summary>
        /// <param name="logger">Logger to invoke the Log call on</param>
        /// <param name="message">Message to pass to the targets</param>
        /// <param name="e">The exception that occurred</param>
        /// <param name="tags">
        /// [Optional] Some tags - optional - if you pass them,
        /// you will also be tagging with <see cref="Logary.Log.ExceptionTag"/>.
        /// </param>
        public static void InfoException(this Logger logger, string message, Exception e, params string[] tags)
        {
            if (logger == null) throw new ArgumentNullException("logger");
            if (message == null) throw new ArgumentNullException("message");
            if (e == null) throw new ArgumentNullException("e");
            logger.Log<object>(message, LogLevel.Info, null, MakeTags(Logary.Log.ExceptionTag, tags), logger.Name, e);
        }

        /// <summary>
        /// Log a debug log message with a collection optional of tags
        /// </summary>
        /// <param name="logger">Logger to invoke the Log call on</param>
        /// <param name="message">Message to pass to the targets</param>
        /// <param name="tags">An optional collection of tags to attach to the log line</param>
        public static void Debug(this Logger logger, string message, params string[] tags)
        {
            logger.Log<object>(message, LogLevel.Debug, null, tags, logger.Name, null);
        }

        /// <summary>
        /// Log a debug log message with a collection optional of tags
        /// </summary>
        /// <param name="logger">Logger to invoke the Log call on</param>
        /// <param name="message">Message to pass to the targets</param>
        /// <param name="e">The exception that occurred</param>
        /// <param name="tags">
        /// [Optional] Some tags - optional - if you pass them,
        /// you will also be tagging with <see cref="Logary.Log.ExceptionTag"/>.
        /// </param>
        public static void DebugException(this Logger logger, string message, Exception e, params string[] tags)
        {
            if (logger == null) throw new ArgumentNullException("logger");
            if (message == null) throw new ArgumentNullException("message");
            if (e == null) throw new ArgumentNullException("e");
            logger.Log<object>(message, LogLevel.Debug, null, MakeTags(Logary.Log.ExceptionTag, tags), logger.Name, e);
        }

        /// <summary>
        /// Log a verbose log message with a collection optional of tags
        /// </summary>
        /// <param name="logger">Logger to invoke the Log call on</param>
        /// <param name="message">Message to pass to the targets</param>
        /// <param name="tags">An optional collection of tags to attach to the log line</param>
        public static void Verbose(this Logger logger, string message, params string[] tags)
        {
            logger.Log<object>(message, LogLevel.Verbose, null, tags, logger.Name, null);
        }

        /// <summary>
        /// Log a verbose log message with a collection optional of tags
        /// </summary>
        /// <param name="logger">Logger to invoke the Log call on</param>
        /// <param name="message">Message to pass to the targets</param>
        /// <param name="e">The exception that occurred</param>
        /// <param name="tags">
        /// [Optional] Some tags - optional - if you pass them,
        /// you will also be tagging with <see cref="Logary.Log.ExceptionTag"/>.
        /// </param>
        public static void VerboseException(this Logger logger, string message, Exception e, params string[] tags)
        {
            if (logger == null) throw new ArgumentNullException("logger");
            if (message == null) throw new ArgumentNullException("message");
            if (e == null) throw new ArgumentNullException("e");
            logger.Log<object>(message, LogLevel.Verbose, null, MakeTags(Logary.Log.ExceptionTag, tags), logger.Name, e);
        }

        static IEnumerable<string> MakeTags(string logaryTag, params string[] tags)
        {
            if (tags == null || tags.Length == 0) return new[] {logaryTag};
            return tags.Union(new[] {logaryTag});
        }
    }
}
