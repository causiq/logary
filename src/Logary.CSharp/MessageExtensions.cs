using System;
using System.Collections.Generic;
using System.Linq;
using NodaTime;

using Logary.DataModel;

namespace Logary
{
    /// <summary>
    /// Extension methods for getting a fluent syntax for writing Messages.
    /// </summary>
    public static class MessageExtensions
    {
        /// <summary>
        /// Sets the format of the message
        /// </summary>
        public static Message SetMessage(this Message msg, string format)
        {
            return MessageModule.SetEvent(format, msg);
        }

        /// <summary>
        /// Set the Message's main exception property
        /// </summary>
        public static Message AddException(this Message msg, Exception ex)
        {
            return MessageModule.AddException(ex, msg);
        }

        /// <summary>
        /// Sets the level of the message
        /// </summary>
        public static Message SetLevel(this Message msg, LogLevel level)
        {
            return MessageModule.SetLevel(level, msg);
        }

        /// <summary>
        /// Sets the service of the message
        /// </summary>
        public static Message SetService(this Message msg, string path)
        {
            return MessageModule.Context.SetService(path, msg);
        }

        /// <summary>
        /// Add the key-value pairs to the data
        /// </summary>
        public static Message SetFields(this Message msg, IEnumerable<KeyValuePair<string, Field>> fields)
        {
            return MessageModule.AddFields(fields.Select(kv => Tuple.Create(PointNameModule.FromString(kv.Key), kv.Value)), msg);
        }

        /// <summary>
        /// Set the Message's timestamp
        /// </summary>
        public static Message SetTimestamp(this Message msg, Instant timestamp)
        {
            return MessageModule.SetTimestamp(timestamp.Ticks, msg);
        }
    }
}