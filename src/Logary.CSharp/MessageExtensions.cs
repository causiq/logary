using System;
using System.Collections.Generic;
using System.Linq;
using NodaTime;

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

        /// <summary>
        /// Sets context values from the given object. The intention behind this method
        /// is that you give it an anonymous object and that that object forms has the properties
        /// whose names you want as keys.
        /// </summary>
        /// <returns>The context object.</returns>
        /// <param name="message">Message to base the new message off of.</param>
        /// <param name="o">Object with properties and data.</param>
        public static Message SetContextFromObject(this Message message, object o)
        {
            return MessageModule.SetContextFromObject(o, message);
        }

        /// <summary>
        /// Sets the context values from the Tuple[string, Value].
        /// </summary>
        /// <returns>The context values.</returns>
        /// <param name="message">Message to add the contxt values to.</param>
        /// <param name="values">Values to add.</param>
        public static Message SetContextValues(this Message message, params Tuple<string, Value>[] values)
        {
            return MessageModule.SetContextValues(values, message);
        }

        /// <summary>
        /// Sets the message name/path.
        /// </summary>
        public static Message SetName(this Message message, PointName name)
        {
            return MessageModule.SetName(name, message);
        }

        /// <summary>
        /// Sets the message name/path.
        /// </summary>
        public static Message SetName(this Message message, string name)
        {
            return MessageModule.SetName(name, message);
        }

        /// <summary>
        /// Appends the nameEnding to the existing message name. If you've logged the
        /// message from a logger, the existing name is likely the name of your logger.
        /// </summary>
        public static Message SetNameEnding(this Message message, string nameEnding)
        {
            return MessageModule.SetNameEnding(nameEnding, message);
        }
    }
}