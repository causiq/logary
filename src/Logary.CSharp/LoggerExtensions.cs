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
        /// C#-oriented-method: Write an Event to the logger. Exposes all parameters in a way that should make
        /// it very easy to use the logging behaviour from C#.
        /// </summary>
        /// <param name="logger">Instance to invoke the extension method on</param>
        /// <param name="level"></param>
        /// <param name="template">A template message to attach to the event</param>
        /// <param name="fields">Data to attach to the log line being sent to the target;
        /// e.g. if using LogStash, these properties will be fields. For performance
        /// improvements, you can send a dictionary, or otherwise you can
        /// send an anonymous object whose public properties are then serialised
        /// as a dictionary.
        /// This is the message that you want to log.
        /// </param>
        /// <param name="timestamp">
        /// When the Message was 'actual'. Optional, defaults to when this method is called.
        /// </param>
        /// <param name="exn">
        /// Any exception associated with the event in this Message. Optional, defaults to null.
        /// </param>
        public static void LogEvent(
            this Logger logger,
            LogLevel level,
            string template,
            object fields = null,
            Instant? timestamp = null,
            Exception exn = null)
        {
            if (logger == null) throw new ArgumentNullException("logger");
            if (level == null) throw new ArgumentNullException("level");
            if (template == null) throw new ArgumentNullException("template");

            var msg = MessageModule.Event(level, template);
            if (fields != null) msg = msg.SetFieldsFromObject(fields);
            if (timestamp != null) msg = msg.SetTimestamp(timestamp.Value);
            if (exn != null) msg = msg.AddException (exn);

            var inLogary = logger.LogWithTimeout(msg).ToTask();
            inLogary.Wait(); // ignore Promise<unit>
        }

        /// <summary>
        /// C#-oriented-method: Write an Event to the logger. Exposes all parameters in a way that should make
        /// it very easy to use the logging behaviour from C#.
        /// </summary>
        /// <param name="logger"></param>
        /// <param name="level"></param>
        /// <param name="template">
        /// This is the message that you want to log.
        /// It's worth noting that a message without string.Format-ed parameters, is more equal
        /// across time, and if you have custom data you want to pass, you should rather set
        /// that data on the 'data' property of Message (with SetField and SetContext*).</param>
        /// <param name="setterTransformer">
        /// There are extension methods in this library that go towards creating new instances
        /// of Message that you can use to change the value inside this function.
        /// </param>
        public static void LogEvent(
            this Logger logger,
            LogLevel level,
            string template,
            Func<Message, Message> setterTransformer)
        {
            if (logger == null) throw new ArgumentNullException("logger");
            if (level == null) throw new ArgumentNullException("level");
            if (template == null) throw new ArgumentNullException("template");
            if (setterTransformer == null) throw new ArgumentNullException("setterTransformer");

            var message = MessageModule.Event(level, template);
            var messageNext = setterTransformer(message);
            var inLogary = logger.LogWithTimeout(messageNext).ToTask();
            inLogary.Wait(); // ignore Promise<unit>
        }

        /// <summary>
        /// Use to time the time it takes to execute function <c>f</c>.
        /// Logging is a Gauge in the unit seconds
        /// </summary>
        public static T Time<T>(this Logger logger, Func<T> f)
        {
            if (logger == null) throw new ArgumentNullException("logger");
            if (f == null) throw new ArgumentNullException("f");

            var res = LoggerModule.Time<Microsoft.FSharp.Core.Unit, T>(logger, CSharp.ToFSharpFunc(f)).Invoke(null);
            res.Item2.ToTask().Wait();
            return res.Item1;
        }

        /// <summary>
        /// Use to time the time it takes to execute action <c>a</c>.
        /// </summary>
        public static void Time(this Logger logger, Action action)
        {
            if (logger == null) throw new ArgumentNullException("logger");
            if (action == null) throw new ArgumentNullException("action");
            Time(logger, () => {
                action();
                return 0;
            });
        }

        /// <summary>
        /// Use to time the time it takes to execute function <c>f</c>. The subPath can be your function name, for
        /// example.
        /// </summary>
        public static T TimePath<T>(this Logger logger, string subPath, Func<T> f, LogLevel level = null)
        {
            if (logger == null) throw new ArgumentNullException("logger");
            if (subPath == null) throw new ArgumentNullException("subPath");
            if (f == null) throw new ArgumentNullException("f");
            var res = LoggerModule.Time<Microsoft.FSharp.Core.Unit, T>(logger, subPath, CSharp.ToFSharpFunc(f)).Invoke(null);
            CSharp.ToTask<Microsoft.FSharp.Core.Unit>(res.Item2).Wait();
            return res.Item1;
        }

        /// <summary>
        /// Log an Event with the template string passed, in <see cref="string.Format(string,object)"/>-style.
        /// </summary>
        /// <param name="logger">The logger to invoke the Log call on</param>
        /// <param name="level">The level that the log format is at</param>
        /// <param name="formatStringMessage">Message to log, may contain C#-esque format placeholders.</param>
        /// <param name="args">Arguments to the format string</param>
        public static void LogEventFormat(this Logger logger, LogLevel level, string formatStringMessage, params object[] args)
        {
            if (logger == null) throw new ArgumentNullException("logger");
            if (level == null) throw new ArgumentNullException("level");
            if (formatStringMessage == null) throw new ArgumentNullException("formatStringMessage");

            var message = MessageModule.EventFormat(level, formatStringMessage, args);
            var inLogary = CSharp.ToTask(logger.LogWithTimeout(message));
            inLogary.Wait(); // ignore Promise<unit>
        }
    }
}