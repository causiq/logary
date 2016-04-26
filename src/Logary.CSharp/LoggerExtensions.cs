using System;
using System.Collections.Generic;
using System.Linq;
using NodaTime;

using Logary;
using Logary.Internals;

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
        /// <param name="fields">Data to attach to the log line being sent to the target;
        /// e.g. if using LogStash, these properties will be fields. For performance
        /// improvements, you can send a dictionary, or otherwise you can
        /// send an anonymous object whose public properties are then serialised
        /// as a dictionary.
        /// This is the message that you want to log.
        /// It's worth noting that a message without string.Format-ed parameters, is more equal
        /// across time, and if you have custom data you want to pass, you should rather set
        /// that data on the 'data' property of LogLine (with SetData and SetDatas).
        /// </param>
        /// <param name="timestamp">
        /// When the log entry was given; optional, defaults to when this method
        /// is called
        /// </param>
        public static void Log(
            this Logger logger,
            LogLevel level,
            string message,
            object fields,
            Instant? timestamp = null)
        {
            if (logger == null) throw new ArgumentNullException("logger");
            if (level == null) throw new ArgumentNullException("level");
            if (message == null) throw new ArgumentNullException("message");

            var msg = MessageModule.Event(level, message);
            if (fields != null) msg.SetFieldsFromObject(fields);
            if (timestamp != null) msg.SetTimestamp(timestamp.Value);

            logger.log(msg).Start();
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
            Func<Message, Message> setterTransformer)
        {
            if (logger == null) throw new ArgumentNullException("logger");
            if (level == null) throw new ArgumentNullException("level");
            if (message == null) throw new ArgumentNullException("message");
            if (setterTransformer == null) throw new ArgumentNullException("setterTransformer");

            var line = MessageModule.Event(level, message);
            logger.log(setterTransformer(line)).Start();
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
            if (logger == null) throw new ArgumentNullException("logger");
            if (level == null) throw new ArgumentNullException("level");
            if (formatStringMessage == null) throw new ArgumentNullException("formatStringMessage");

            logger.log(MessageModule.EventFormat(level, formatStringMessage, args)).Start();
        }

        /// <summary>
        /// Log a fatal log message
        /// </summary>
        /// <param name="logger">logger to invoke the Log call on</param>
        /// <param name="message">Message to pass to the targets</param>
        public static void Fatal(this Logger logger, string message)
        {
            if (logger == null) throw new ArgumentNullException("logger");
            if (message == null) throw new ArgumentNullException("message");

            logger.Log(message, LogLevel.Fatal).Start();
        }

        /// <summary>
        /// Log a fatal log message with an exception
        /// </summary>
        /// <param name="logger">logger to invoke the Log call on</param>
        /// <param name="message">Message to pass to the targets</param>
        /// <param name="e">The exception that occurred</param>
        public static void FatalException(this Logger logger, string message, Exception e)
        {
            if (logger == null) throw new ArgumentNullException("logger");
            if (message == null) throw new ArgumentNullException("message");
            if (e == null) throw new ArgumentNullException("e");

            logger.log(MessageModule.Event(LogLevel.Fatal, message).AddException(e)).Start();
        }

        /// <summary>
        /// Log a error log message
        /// </summary>
        /// <param name="logger">logger to invoke the Log call on</param>
        /// <param name="message">Message to pass to the targets</param>
        public static void Error(this Logger logger, string message)
        {
            if (logger == null) throw new ArgumentNullException("logger");
            if (message == null) throw new ArgumentNullException("message");

            logger.Log(message, LogLevel.Error).Start();
        }

        /// <summary>
        /// Log a error log message
        /// </summary>
        /// <param name="logger">[NotNull] logger to invoke the Log call on</param>
        /// <param name="message">[NotNull] Message to pass to the targets</param>
        /// <param name="e">[NotNull] The exception that occurred</param>
        public static void ErrorException(this Logger logger, string message, Exception e)
        {
            if (logger == null) throw new ArgumentNullException("logger");
            if (message == null) throw new ArgumentNullException("message");
            if (e == null) throw new ArgumentNullException("e");

            logger.log(MessageModule.Event(LogLevel.Error, message).AddException(e)).Start();
        }

        /// <summary>
        /// Log a warn log message
        /// </summary>
        /// <param name="logger">logger to invoke the Log call on</param>
        /// <param name="message">Message to pass to the targets</param>
        public static void Warn(this Logger logger, string message)
        {
            if (logger == null) throw new ArgumentNullException("logger");
            if (message == null) throw new ArgumentNullException("message");

            logger.Log(message, LogLevel.Warn).Start();
        }

        /// <summary>
        /// Log a warn log message
        /// </summary>
        /// <param name="logger">logger to invoke the Log call on</param>
        /// <param name="message">Message to pass to the targets</param>
        /// <param name="e">The exception that occurred</param>
        public static void WarnException(this Logger logger, string message, Exception e)
        {
            if (logger == null) throw new ArgumentNullException("logger");
            if (message == null) throw new ArgumentNullException("message");
            if (e == null) throw new ArgumentNullException("e");

            logger.log(MessageModule.Event(LogLevel.Warn, message).AddException(e)).Start();
        }

        /// <summary>
        /// Log a info log message
        /// </summary>
        /// <param name="logger">logger to invoke the Log call on</param>
        /// <param name="message">Message to pass to the targets</param>
        public static void Info(this Logger logger, string message)
        {
            if (logger == null) throw new ArgumentNullException("logger");
            if (message == null) throw new ArgumentNullException("message");

            logger.Log(message, LogLevel.Info).Start();
        }

        /// <summary>
        /// Log a info log message
        /// </summary>
        /// <param name="logger">logger to invoke the Log call on</param>
        /// <param name="message">Message to pass to the targets</param>
        /// <param name="e">The exception that occurred</param>
        public static void InfoException(this Logger logger, string message, Exception e)
        {
            if (logger == null) throw new ArgumentNullException("logger");
            if (message == null) throw new ArgumentNullException("message");
            if (e == null) throw new ArgumentNullException("e");

            logger.log(MessageModule.Event(LogLevel.Info, message).AddException(e)).Start();
        }

        /// <summary>
        /// Log a debug log message
        /// </summary>
        /// <param name="logger">logger to invoke the Log call on</param>
        /// <param name="message">Message to pass to the targets</param>
        public static void Debug(this Logger logger, string message)
        {
            logger.Log(message, LogLevel.Debug).Start();
        }

        /// <summary>
        /// Log a debug log message
        /// </summary>
        /// <param name="logger">logger to invoke the Log call on</param>
        /// <param name="message">Message to pass to the targets</param>
        /// <param name="e">The exception that occurred</param>
        public static void DebugException(this Logger logger, string message, Exception e)
        {
            if (logger == null) throw new ArgumentNullException("logger");
            if (message == null) throw new ArgumentNullException("message");
            if (e == null) throw new ArgumentNullException("e");

            logger.log(MessageModule.Event(LogLevel.Debug, message).AddException(e)).Start();
        }

        /// <summary>
        /// Log a verbose log message
        /// </summary>
        /// <param name="logger">logger to invoke the Log call on</param>
        /// <param name="message">Message to pass to the targets</param>
        public static void Verbose(this Logger logger, string message)
        {
            logger.Log(message, LogLevel.Verbose).Start();
        }

        /// <summary>
        /// Log a verbose log message
        /// </summary>
        /// <param name="logger">logger to invoke the Log call on</param>
        /// <param name="message">Message to pass to the targets</param>
        /// <param name="e">The exception that occurred</param>
        public static void VerboseException(this Logger logger, string message, Exception e)
        {
            if (logger == null) throw new ArgumentNullException("logger");
            if (message == null) throw new ArgumentNullException("message");
            if (e == null) throw new ArgumentNullException("e");

            logger.log(MessageModule.Event(LogLevel.Verbose, message).AddException(e)).Start();
        }
    }
}
