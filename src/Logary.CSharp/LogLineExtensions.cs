using System;
using System.Collections.Generic;
using System.Linq;
using NodaTime;

namespace Logary
{
    /// <summary>
    /// Extension methods for getting a fluent syntax for writing LogLines.
    /// </summary>
    public static class LogLineExtensions
    {
        /// <summary>
        /// Sets the message of the log line
        /// </summary>
        public static LogLine SetMessage(this LogLine line, string message)
        {
            return LogLineModule.SetMsg(message, line);
        }

        /// <summary>
        /// Set the LogLine's main exception property 
        /// </summary>
        public static LogLine SetExn(this LogLine line, Exception ex)
        {
            return LogLineModule.SetExn(ex, line);
        }

        /// <summary>
        /// Sets the level of the log line 
        /// </summary>
        public static LogLine SetLevel(this LogLine line, LogLevel level)
        {
            return LogLineModule.SetLevel(level, line);
        }

        /// <summary>
        /// Sets the path of the log line 
        /// </summary>
        public static LogLine SetPath(this LogLine line, string path)
        {
            return LogLineModule.SetPath(path, line);
        }

        /// <summary>
        /// Add a tag 'tag' to the log line 'line'. 
        /// </summary>
        public static LogLine SetTag(this LogLine line, string tag)
        {
            return LogLineModule.SetTag(tag, line);
        }

        /// <summary>
        /// Add a key-value pair to the data 
        /// </summary>
        public static LogLine SetData(this LogLine line, string key, object data)
        {
            return LogLineModule.SetData(key, data, line);
        }

        /// <summary>
        /// Add the key-value pairs to the data 
        /// </summary>
        public static LogLine SetDatas(this LogLine line, IEnumerable<KeyValuePair<string, object>> values)
        {
            return LogLineModule.SetDatas(values.Select(kv => Tuple.Create(kv.Key, kv.Value)), line);
        }

        /// <summary>
        /// Set the LogLine's timestamp 
        /// </summary>
        public static LogLine SetTimestamp(this LogLine line, Instant timestamp)
        {
            return LogLineModule.SetTimestamp(timestamp, line);
        }
    }
}