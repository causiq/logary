using System;
using System.Collections.Generic;
using System.Linq;
using NodaTime;

using Logary;

namespace Logary
{
    /// <summary>
    /// Extension methods for getting a fluent syntax for writing Messages.
    /// </summary>
    public static class MessageExtensions
    {
        /// <summary>
        /// Sets the format of the message.
        /// </summary>
        public static Message SetEvent(this Message msg, string format)
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
        /// Add the key-value pairs to the data
        /// </summary>
        public static Message SetFieldsFromObject(this Message msg, IEnumerable<KeyValuePair<string, Field>> fields)
        {
             return MessageModule.SetFieldsFromObject(fields.Select(kv => Tuple.Create(PointNameModule.Parse(kv.Key), kv.Value)), msg);
        }

        /// <summary>
        /// Serializes the object into fields and adds them to the message
        /// </summary>
        public static Message SetFieldsFromObject(this Message msg, object obj)
        {
            return MessageModule.SetFieldsFromObject(obj, msg);
        }

        /// <summary>
        /// Set the Message's timestamp.
        /// </summary>
        public static Message SetTimestamp(this Message msg, Instant timestamp)
        {
            return MessageModule.SetTicksEpoch(timestamp.Ticks, msg);
        }

        /// <summary>
        /// Set the Message's timestamp.
        /// </summary>
        public static Message SetTimestamp(this Message msg, DateTimeOffset timestamp)
        {
            return SetTimestamp(msg, Instant.FromDateTimeOffset(timestamp));
        }

        /// <summary>
        /// Gets the timestamp as an Instant, which is on the UTC timeline.
        /// </summary>
        /// <returns>The timestamp.</returns>
        /// <param name="msg">Message.</param>
        public static Instant GetTimestamp(this Message msg)
        {
            return Instant.FromTicksSinceUnixEpoch(msg.timestampTicks);
        }
    }
}