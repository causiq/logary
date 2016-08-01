//-----------------------------------------------------------------------
// <copyright file="TraceHeader.cs" company="Bazinga Technologies Inc.">
//     Copyright (C) 2016 Bazinga Technologies Inc.
// </copyright>
//-----------------------------------------------------------------------

using System;
using System.Text;

namespace Logary.Zipkin
{
    /// <summary>
    /// A structure containing all of the data necessary to identify span and it's trace among others.
    /// </summary>
    public struct TraceHeader : IEquatable<TraceHeader>, IComparable<TraceHeader>
    {
        /// <summary>
        /// Name of a HTTP header that could contain <see cref="TraceId"/> information encoded as hex string of 64-bit unsigned integer.
        /// </summary>
        public const string TraceIdHttpHeaderName = "X-B3-TraceId";

        /// <summary>
        /// Name of a HTTP header that could contain <see cref="SpanId"/> information encoded as hex string of 64-bit unsigned integer.
        /// </summary>
        public const string SpanIdHttpHeaderName = "X-B3-SpanId";

        /// <summary>
        /// Name of a HTTP header that could contain <see cref="ParentId"/> information encoded as hex string of 64-bit unsigned integer.
        /// </summary>
        public const string ParentIdHttpHeaderName = "X-B3-ParentSpanId";

        /// <summary>
        /// Name of a HTTP header that could contain <see cref="IsDebug"/> information encoded as 1 (true) or 0 (false).
        /// </summary>
        public const string SampledHttpHeaderName = "X-B3-Sampled";

        /// <summary>
        /// The overall ID of the trace. Every span in a trace will share this ID.
        /// </summary>
        public readonly ulong TraceId;

        /// <summary>
        /// The ID for a particular span. This may or may not be the same as the trace id.
        /// </summary>
        public readonly ulong SpanId;

        /// <summary>
        /// This is an optional ID that will only be present on child spans. 
        /// That is the span without a parent id is considered the root of the trace.
        /// </summary>
        public readonly ulong? ParentId;

        /// <summary>
        /// Marks current span with debug flag.
        /// </summary>
        public readonly bool IsDebug;

        public TraceHeader(ulong traceId, ulong spanId, ulong? parentId = null, bool isDebug = false)
        {
            TraceId = traceId;
            SpanId = spanId;
            ParentId = parentId;
            IsDebug = isDebug;
        }
        
        /// <summary>
        /// Creates a trace header for the new span being a child of the span identified by current trace header.
        /// </summary>
        public TraceHeader Child(ulong childId) => new TraceHeader(TraceId, childId, SpanId, IsDebug);

        public bool Equals(TraceHeader other)
        {
            return other.TraceId == TraceId && other.SpanId == SpanId && other.ParentId == ParentId;
        }

        public override bool Equals(object obj)
        {
            return obj is TraceHeader && Equals((TraceHeader)obj);
        }

        public override int GetHashCode()
        {
            unchecked
            {
                var hashCode = TraceId.GetHashCode();
                hashCode = (hashCode*397) ^ SpanId.GetHashCode();
                hashCode = (hashCode*397) ^ ParentId.GetHashCode();
                return hashCode;
            }
        }

        public int CompareTo(TraceHeader other)
        {
            var tcmp = TraceId.CompareTo(other.TraceId);
            if (tcmp == 0)
            {
                var pcmp = ParentId.GetValueOrDefault(0).CompareTo(other.ParentId.GetValueOrDefault(0));
                if (pcmp == 0)
                {
                    return SpanId.CompareTo(other.SpanId);
                }
                else return pcmp;
            }
            else return tcmp;
        }

        public override string ToString()
        {
            var sb = new StringBuilder("TraceHeader(traceId:").Append(TraceId).Append(", spanId: ").Append(SpanId);

            if (ParentId.HasValue)
            {
                sb.Append(", parentId: ").Append(ParentId);
            }

            sb.Append(")");
            return sb.ToString();
        }
    }
}