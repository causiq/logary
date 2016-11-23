using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Diagnostics;
using System.Linq;
using System.Reflection;
using System.Runtime.CompilerServices;
using System.Text;
using System.Threading.Tasks;

namespace Logary.CSharp.Facade
{

    public static class ILoggerEx
    {
        public static ITimeScope TimeScope(this ILogger me, string caller, object msgPayload = null)
        {
            if (caller == null) throw new ArgumentNullException(nameof(caller));
            return new StopwatchTimeScope(me, caller);
        }

        public static ITimeScope TimeScope(this ILogger me, string caller, Func<LogMessage, LogMessage> transform)
        {
            if (caller == null) throw new ArgumentNullException(nameof(caller));
            return new StopwatchTimeScope(me, caller, transform);
        }

        public static Func<T> Time<T>(this ILogger me, Func<T> trans, string caller, Func<LogMessage, LogMessage> transform = null)
        {
            if (caller == null) throw new ArgumentNullException(nameof(caller));
            return () =>
            {
                using (new StopwatchTimeScope(me, caller, transform))
                    return trans();
            };
        }

        public static Action Time(this ILogger me, Action act, string caller, Func<LogMessage, LogMessage> transform = null)
        {
            if (caller == null) throw new ArgumentNullException(nameof(caller));
            return () =>
            {
                using (new StopwatchTimeScope(me, caller, transform))
                    act();
            };
        }
    }
    public interface ILogger
    {
        string[] Name { get; }

        void Log(LogMessage payload);

        Task<Task> LogWithAck(LogLevel level, Func<LogMessage, LogMessage> trans);

        Task Log(LogLevel level, Func<LogMessage, LogMessage> trans);
    }

    public static class Literals
    {
        public static string FieldExnKey = "exn";
    }

    public enum LogLevel
    {
        Verbose,
        Debug,
        Info,
        Warning,
        Error,
        Fatal
    }

    public static class Units
    {
        public const string Scalar = "Scalar";
        public const string Seconds = "Seconds";
        public const string Seconds_Ms = "Seconds_Ms";
        public const string Seconds_Us = "Seconds_Us";
        public const string Seconds_Ns = "Seconds_Ns";
    }

    public interface ILoggingConfig
    {
        long GetTimestamp();

        ILogger GetLogger(string[] name);

        object ConsoleSemaphore { get; }
    }

    public class NullLogger : LoggerVersionBridge

    {
        private readonly string[] _name = { "Logary", "Facade", "CSharp", "NullLogger" };

        private readonly Task _done = Task.Delay(0);
        private readonly Task<Task> _doneDone;

        public NullLogger()
        {
            _doneDone = new Task<Task>(() => _done);
        }

        public override string[] Name => _name;

        public override void Log(LogMessage payload)
        {
        }

        public override Task<Task> LogWithAck(LogLevel level, Func<LogMessage, LogMessage> trans)
        {
            return _doneDone;
        }

        public override Task Log(LogLevel level, Func<LogMessage, LogMessage> trans)
        {
            return _done;
        }
    }

    public static class Global
    {
        internal static object ConsoleSemaphore { get; } = new object();

        class DefaultConfigImpl
            : ILoggingConfig
        {
            readonly object _consoleSemaphore;
            readonly Func<string[], ILogger> _getLoggerFn;
            readonly Func<long> _getTsFn;

            public DefaultConfigImpl()
            {
                _consoleSemaphore = Global.ConsoleSemaphore;
                _getTsFn = () => DateTimeOffset.UtcNow.ToTimestamp();
                _getLoggerFn = _ => new NullLogger();
            }

            public DefaultConfigImpl(ILoggingConfig baseConfig, Func<string[], ILogger> getLoggerFn)
            {
                if (getLoggerFn == null) throw new ArgumentNullException(nameof(getLoggerFn));
                _consoleSemaphore = baseConfig.ConsoleSemaphore;
                _getTsFn = baseConfig.GetTimestamp;
                _getLoggerFn = getLoggerFn;
            }

            long ILoggingConfig.GetTimestamp()
            {
                return _getTsFn();
            }

            ILogger ILoggingConfig.GetLogger(string[] name)
            {
                return _getLoggerFn(name);
            }

            object ILoggingConfig.ConsoleSemaphore => _consoleSemaphore;
        }

        public static ILoggingConfig DefaultConfig { get; } = new DefaultConfigImpl();

        // ReSharper disable once FieldCanBeMadeReadOnly.Local
        static ILoggingConfig _config = DefaultConfig;

        internal class Flyweight : LoggerVersionBridge

        {
            static readonly object Unit = new object();
            private readonly ILogger _initialLogger;
            private readonly Lazy<ILogger> _actualLogger;
            private readonly string[] _name;

            public Flyweight(string[] name)
            {
                _name = name;
                _initialLogger = _config.GetLogger(name);
                _actualLogger = new Lazy<ILogger>(() => _config.GetLogger(name),
                    LazyThreadSafetyMode.ExecutionAndPublication);
            }

            T WithLogger<T>(Func<ILogger, T> callback)
            {
                var logger =
                   ReferenceEquals(_config, DefaultConfig)
                       ? _initialLogger
                       : _actualLogger.Value;

                return callback(logger);
            }

            void WithLogger(Action<ILogger> callback)
            {
                WithLogger(logger =>
                {
                    callback(logger);
                    return Unit;
                });
            }

            LogMessage EnsureName(LogMessage message)
            {
                if (message.Name.Length == 0)
                {
                    return message.SetName(_name);
                }
                return message;
            }

            public override string[] Name => _name;

            public override void Log(LogMessage payload)
            {
                WithLogger(logger => logger.Log(EnsureName(payload)));
            }

            public override Task<Task> LogWithAck(LogLevel level, Func<LogMessage, LogMessage> trans)
            {
                return WithLogger(logger => logger.LogWithAck(level, x => trans(EnsureName(x))));
            }

            public override Task Log(LogLevel level, Func<LogMessage, LogMessage> trans)
            {
                return WithLogger(logger => logger.Log(level, x => trans(EnsureName(x))));
            }
        }

        public static ILogger GetStaticLogger(string[] name)
        {
            if (name == null) throw new ArgumentNullException(nameof(name));
            return new Flyweight(name);
        }

        public static long GetTimestamp()
        {
            return _config.GetTimestamp();
        }

        /// <summary>
        /// Initialise the Logary facade globally.
        /// </summary>
        /// <param name="config"></param>
        public static void Initialise(ILoggingConfig config)
        {
            if (config == null) throw new ArgumentNullException(nameof(config));
            _config = config;
        }

        public static void InitialiseIfDefault(ILoggingConfig config)
        {
            if (config == null) throw new ArgumentNullException(nameof(config));
            if (ReferenceEquals(config, DefaultConfig))
            {
                Initialise(config);
            }
        }

        // helpers for updating the configuration

        public static ILoggingConfig WithGetLogger(this ILoggingConfig me, Func<string[], ILogger> getLoggerFn)
        {
            return new DefaultConfigImpl(me, getLoggerFn);
        }
    }
    public static class Logging
    {
        [MethodImpl(MethodImplOptions.NoInlining)]
        public static ILogger GetCurrentLogger()
        {
            string className;
            Type declaringType;
            int framesToSkip = 1;

            do
            {
                StackFrame frame = new StackFrame(framesToSkip, false);
                MethodBase method = frame.GetMethod();
                declaringType = method.DeclaringType;
                if (declaringType == null)
                {
                    className = method.Name;
                    break;
                }

                framesToSkip++;
                className = declaringType.FullName;
            } while (declaringType.Module.Name.Equals("mscorlib.dll", StringComparison.OrdinalIgnoreCase));

            return new Global.Flyweight(className.Split(new[] { '.' }));
        }
    }

    public struct LogMessage
    {
        public string[] Name { get; private set; }
        public LogLevel Level { get; private set; }
        public PointValue Value { get; private set; }
        public ReadOnlyDictionary<string, object> Fields { get; private set; }
        public long Timestamp { get; private set; }

        public LogMessage(string[] name, LogLevel level, PointValue value, ReadOnlyDictionary<string, object> fields, long timestamp)
        {
            Name = name;
            Level = level;
            Value = value;
            Fields = fields;
            Timestamp = timestamp;
        }

        public LogMessage(LogMessage msg, PointValue value)
        {
            this.Name = msg.Name;
            this.Level = msg.Level;
            this.Value = value;
            this.Fields = msg.Fields;
            this.Timestamp = msg.Timestamp;

        }

        public LogMessage(LogMessage msg, string[] name)
        {
            this.Name = name;
            this.Level = msg.Level;
            this.Value = msg.Value;
            this.Fields = msg.Fields;
            this.Timestamp = msg.Timestamp;

        }
        public LogMessage(LogMessage msg, LogLevel level)
        {
            this.Name = msg.Name;
            this.Level = level;
            this.Value = msg.Value;
            this.Fields = msg.Fields;
            this.Timestamp = msg.Timestamp;

        }
        public LogMessage(LogMessage msg, ReadOnlyDictionary<string, object> fields)
        {
            this.Name = msg.Name;
            this.Level = msg.Level;
            this.Value = msg.Value;
            this.Fields = fields;
            this.Timestamp = msg.Timestamp;

        }
        public LogMessage(LogMessage msg, long timestamp)
        {
            this.Name = msg.Name;
            this.Level = msg.Level;
            this.Value = msg.Value;
            this.Fields = msg.Fields;
            this.Timestamp = timestamp;

        }


    }

    static class DateTimeExtensions
    {
        /// <summary>
        /// Gets the timestamp for the date time in nanoseconds since unix epoch, as seen from the UTC
        /// time zone.
        /// </summary>
        public static long ToTimestamp(this DateTime me)
        {
            return (me.Ticks - new DateTime(1970, 01, 01, 00, 00, 00, DateTimeKind.Utc).Ticks) * 100;
        }
    }

    static class DateTimeOffsetExtensions
    {
        /// <summary>
        /// Gets the timestamp for the date time offset in nanoseconds since unix epoch, as seen from the UTC
        /// time zone.
        /// </summary>
        public static long ToTimestamp(this DateTimeOffset me)
        {
            return (me.Ticks - new DateTimeOffset(1970, 01, 01, 0, 0, 0, TimeSpan.Zero).Ticks) * 100;
        }
    }

    static class TimestampExtensions
    {
        static DateTimeOffset _epochStartDt = new DateTimeOffset(1970, 1, 1, 0, 0, 0, TimeSpan.Zero);
        static DateTimeOffset _epochStartDto = new DateTimeOffset(1970, 1, 1, 0, 0, 0, TimeSpan.Zero);

        public static DateTimeOffset ToDateTimeOffset(this long epochNanoSeconds)
        {
            return new DateTimeOffset(epochNanoSeconds / 100L + _epochStartDto.Ticks, TimeSpan.Zero);
        }

        public static DateTime ToDateTime(this long epochNanoSeconds)
        {
            return new DateTime(epochNanoSeconds / 100L + _epochStartDt.Ticks, DateTimeKind.Utc);
        }
    }

    public abstract class PointValue
    {
        public sealed class Event
            : PointValue
        {
            public readonly string Template;

            internal Event(string template)
            {
                Template = template;
            }
        }

        public sealed class Gauge : PointValue
        {
            public readonly long Value;
            public readonly string Unit;

            internal Gauge(long @value, string unit)
            {
                Value = value;
                Unit = unit;
            }
        }

        public static PointValue FromEvent(string template)
        {
            if (template == null) throw new ArgumentNullException(nameof(template));
            return new Event(template);
        }

        public static PointValue FromGauge(long @value, string unit)
        {
            if (unit == null) throw new ArgumentNullException(nameof(unit));
            return new Gauge(@value, unit);
        }
    }

    public static class PointValueExtensions
    {
        public static T Match<T>(this PointValue @value, Func<PointValue.Event, T> onEvent, Func<PointValue.Gauge, T> onGauge)
        {
            if (@value is PointValue.Event)
            {
                return onEvent(@value as PointValue.Event);
            }
            return onGauge(@value as PointValue.Gauge);
        }
    }

    public static class ReadOnlyDictExtensions
    {
        public static ReadOnlyDictionary<K, T> Add<K, T>(this ReadOnlyDictionary<K, T> me, K key, T value)
        {
            return new ReadOnlyDictionary<K, T>(new Dictionary<K, T>(me) { [key] = value });
        }

        public static ReadOnlyDictionary<K, T> Add<K, T>(this ReadOnlyDictionary<K, T> me, IList<Tuple<K, T>> items)
        {
            var dict = new Dictionary<K, T>(me);

            foreach (var i in items)
                dict[i.Item1] = i.Item2;

            return new ReadOnlyDictionary<K, T>(dict);
        }

        public static ReadOnlyDictionary<K, T> Add<K, T>(this ReadOnlyDictionary<K, T> me, IEnumerable<KeyValuePair<K, T>> items)
        {
            var dict = new Dictionary<K, T>(me);

            foreach (var i in items)
                dict[i.Key] = i.Value;

            return new ReadOnlyDictionary<K, T>(dict);
        }
    }

    public static class LogMessageExtensions
    {
        public static LogMessage SetName(this LogMessage msg, string[] name)
        {
            return new LogMessage(msg, name: name);
        }

        public static LogMessage SetNameEnding(this LogMessage msg, string caller)
        {
            if (string.IsNullOrWhiteSpace(caller)) return msg;
            var names = new List<string>(msg.Name);
            names.Add(caller);
            return new LogMessage(msg, name: names.ToArray());
        }

        public static LogMessage SetGuage(this LogMessage msg, long value, string unit)
        {
            return new LogMessage(msg, value: PointValue.FromGauge(value, unit));
        }

        /// <summary>
        /// Sets the format of the LogMessage.
        /// </summary>
        public static LogMessage SetEvent(this LogMessage msg, string format)
        {
            return new LogMessage(msg, value: PointValue.FromEvent(format));
        }

        /// <summary>
        /// Set the LogMessage's main exception property
        /// </summary>
        public static LogMessage AddException(this LogMessage msg, Exception ex)
        {
            return new LogMessage(msg, fields: msg.Fields.Add(Literals.FieldExnKey, ex));
        }

        /// <summary>
        /// Adds a tag to the tag array inside the message.
        /// </summary>
        /// <param name="msg"></param>
        /// <param name="tag"></param>
        /// <returns></returns>
        public static LogMessage AddTag(this LogMessage msg, string tag)
        {
            object existing;
            if (msg.Fields.TryGetValue(Fields.Tags, out existing))
            {
                var tags = new List<string>((string[])existing);
                tags.Add(tag);
                return msg.SetField(Fields.Tags, tags.ToArray());
            }
            return msg.SetField(Fields.Tags, new[] { tag });
        }

        /// <summary>
        /// Sets the level of the LogMessage
        /// </summary>
        public static LogMessage SetLevel(this LogMessage msg, LogLevel level)
        {
            return new LogMessage(msg, level: level);
        }

        /// <summary>
        /// Add the key-value pairs to the data
        /// </summary>
        public static LogMessage SetFieldsFromObject(this LogMessage msg, IList<Tuple<string, object>> fields)
        {
            return new LogMessage(msg, msg.Fields.Add(fields));
        }

        public static LogMessage SetFieldsFromObject(this LogMessage msg, object hedniskt)
        {
            return new LogMessage(msg, new ReadOnlyDictionary<string, object>(hedniskt.ToDictionary()));
        }

        /// <summary>
        /// Add the key-value pairs to the data
        /// </summary>
        public static LogMessage SetFields(this LogMessage msg, IDictionary<string, object> fields)
        {
            return new LogMessage(msg, msg.Fields.Add(fields));
        }

        /// <summary>
        /// Add the key-value pairs to the data
        /// </summary>
        public static LogMessage SetField(this LogMessage msg, string key, object value)
        {
            return new LogMessage(msg, msg.Fields.Add(key, value));
        }

        /// <summary>
        /// Set the LogMessage's timestamp.
        /// </summary>
        public static LogMessage SetTimestamp(this LogMessage msg, DateTime timestamp)
        {
            return new LogMessage(msg, timestamp.ToTimestamp());
        }

        /// <summary>
        /// Set the LogMessage's timestamp.
        /// </summary>
        public static LogMessage SetTimestamp(this LogMessage msg, DateTimeOffset timestamp)
        {
            return new LogMessage(msg, timestamp.ToTimestamp());
        }

        /// <summary>
        /// Gets the timestamp as an Instant, which is on the UTC timeline.
        /// </summary>
        /// <returns>The timestamp.</returns>
        /// <param name="msg">LogMessage.</param>
        public static DateTimeOffset GetTimestamp(this LogMessage msg)
        {
            return msg.Timestamp.ToDateTimeOffset();
        }

        /// <summary>
        /// Sets the context values from the Tuple[string, Value].
        /// </summary>
        /// <returns>The context values.</returns>
        public static LogMessage SetContextValues(this LogMessage msg, params Tuple<string, object>[] values)
        {
            return new LogMessage(msg, fields: msg.Fields.Add(values));
        }

        /// <summary>
        /// Sets the context values from the key and value.
        /// </summary>
        /// <returns>The context values.</returns>
        public static LogMessage SetContextValue(this LogMessage msg, string key, object value)
        {
            return new LogMessage(msg, fields: msg.Fields.Add(key, value));
        }
    }

    public static class ObjectToDictionary
    {
        private static readonly ConcurrentDictionary<Type, Func<object, IDictionary<string, object>>> Converters =
            new ConcurrentDictionary<Type, Func<object, IDictionary<string, object>>>();

        /// <summary>
        /// Copies all readable properties from an object to a dictionary.
        /// </summary>
        /// <param name="obj">The object.</param>
        /// <returns>A dictionary representation of the object's properties.</returns>
        public static IDictionary<string, object> ToDictionary(this object obj)
        {
            if (obj == null) throw new ArgumentNullException("obj");

            var converter = Converters.GetOrAdd(obj.GetType(), CreateConverter);

            return converter(obj);
        }

        private static Func<object, IDictionary<string, object>> CreateConverter(Type type)
        {
            var toDictionaryMethod = type.GetMethod("ToDictionary", new Type[0]);
            if (toDictionaryMethod != null)
            {
                return obj => toDictionaryMethod.Invoke(obj, null) as IDictionary<string, object>;
            }

            // do reflection once!
            var properties = type.GetProperties().Where(p => p.CanRead && p.GetIndexParameters().Length == 0).ToList();

            return obj =>
            {
                var dictionary = new Dictionary<string, object>(properties.Count);
                foreach (var property in properties)
                {
                    dictionary.Add(property.Name, property.GetValue(obj, null));
                }
                return dictionary;
            };
        }
    }

    public interface ITimeScope : IDisposable
    {
    }

    public class StopwatchTimeScope
        : ITimeScope
    {
        private readonly ILogger _logger;
        private readonly string _caller;
        readonly object _msgPayload;
        private readonly Func<LogMessage, LogMessage> _trans;
        private readonly Stopwatch _sw;

        public StopwatchTimeScope(ILogger logger, string caller, Func<LogMessage, LogMessage> trans = null, object msgPayload = null)
        {
            if (logger == null) throw new ArgumentNullException(nameof(logger));
            if (caller == null) throw new ArgumentNullException(nameof(caller));
            _logger = logger;
            _caller = caller;
            _msgPayload = msgPayload;
            _trans = trans ?? (s => s);
            _sw = Stopwatch.StartNew();
        }

        public void Dispose()
        {
            _logger.Log(LogLevel.Info,
                s =>
                {
                    var msg = s.SetGuage(_sw.ElapsedTicks * 100L, Units.Seconds_Ns).SetNameEnding(_caller);
                    msg = _msgPayload == null ? msg : msg.SetFieldsFromObject(_msgPayload);
                    return _trans(msg);
                });
        }
    }
}
