//-----------------------------------------------------------------------
// <copyright file="Annotation.cs" company="Bazinga Technologies Inc.">
//     Copyright (C) 2016 Bazinga Technologies Inc.
// </copyright>
//-----------------------------------------------------------------------

using System;
using System.Net;

namespace Logary.Zipkin
{
    /// <summary>
    /// An Annotation is used to record an occurance in time.
    /// </summary>
    public struct Annotation : IEquatable<Annotation>
    {
        /// <summary>
        /// Timestamp marking the occurrence of an event.
        /// </summary>
        public readonly DateTime Timestamp;

        /// <summary>
        /// Value holding an information about the annotation. 
        /// See <see cref="AnnotationConstants"/> for some 
        /// build in Zipkin annotation values.
        /// </summary>
        public readonly string Value;

        /// <summary>
        /// Service endpoint.
        /// </summary>
        public readonly IPEndPoint Endpoint;
        
        public Annotation(string value, DateTime timestamp, IPEndPoint endpoint)
        {
            Timestamp = timestamp;
            Value = value;
            Endpoint = endpoint;
        }

        public Annotation(string value, DateTime timestamp) : this()
        {
            Timestamp = timestamp;
            Value = value;
        }

        /// <summary>
        /// Returns a new instance of the <see cref="Annotation"/> with 
        /// <paramref name="timestamp"/> set and all other fields copied 
        /// from current instance.
        /// </summary>
        public Annotation WithTimestamp(DateTime timestamp) => new Annotation(Value, timestamp, Endpoint);

        /// <summary>
        /// Returns a new instance of the <see cref="Annotation"/> with 
        /// <paramref name="endpoint"/> set and all other fields copied 
        /// from current instance.
        /// </summary>
        public Annotation WithEndpoint(IPEndPoint endpoint) => new Annotation(Value, Timestamp, endpoint);

        public bool Equals(Annotation other) => other.Timestamp == Timestamp && Equals(other.Value, Value) && Equals(other.Endpoint, Endpoint);

        public override bool Equals(object obj) => obj is Annotation && Equals((Annotation)obj);

        public override int GetHashCode()
        {
            unchecked
            {
                var hashCode = Timestamp.GetHashCode();
                hashCode = (hashCode * 397) ^ (Value != null ? Value.GetHashCode() : 0);
                hashCode = (hashCode * 397) ^ (Endpoint != null ? Endpoint.GetHashCode() : 0);
                return hashCode;
            }
        }

        public override string ToString() => $"Annotation({Value}, {Timestamp.ToString("O")}, {Endpoint})";
    }
}