using System;
using System.Net;
using Logary.Zipkin;

namespace Logary.Zipkin.Thrift
{
    internal static class ThriftExtensions
    {
        public static Annotation ToThrift(this Zipkin.Annotation annotation)
        {
            return new Annotation(annotation.Timestamp.ToUnixMicroseconds(), annotation.Value, annotation.Endpoint.ToThrift());
        }
        public static BinaryAnnotation ToThrift(this Zipkin.BinaryAnnotation annotation)
        {
            return new BinaryAnnotation(annotation.Key, annotation.Value, annotation.AnnotationType, annotation.Endpoint.ToThrift());
        }

        public static readonly DateTime UnixEpochStart = new DateTime(1970, 1, 1);
        public static long ToUnixMicroseconds(this DateTime date)
        {
            return (date.Ticks - UnixEpochStart.Ticks)/AnnotationConstants.TicksPerMicosecond;
        }

        public static Span ToThrift(this Zipkin.Span span)
        {
            var s = new Span
            {
                TraceId = (long)span.TraceHeader.TraceId,
                Id = (long)span.TraceHeader.SpanId,
                Name = span.Name,
                Debug = span.TraceHeader.IsDebug
            };

            if (span.TraceHeader.ParentId.HasValue) s.ParentId = (long)span.TraceHeader.ParentId.Value;

            s.__isset.annotations = span.Annotations.Count != 0;
            foreach (var annotation in span.Annotations)
            {
                var thrifted = annotation.ToThrift();
                var ep = thrifted.__isset.host ? thrifted.Host : span.Endpoint.ToThrift();
                ep.ServiceName = span.ServiceName;
                thrifted.Host = ep;
                s.Annotations.Add(thrifted);
            }

            s.__isset.binary_annotations = span.BinaryAnnotations.Count != 0;
            foreach (var binaryAnnotation in span.BinaryAnnotations)
            {
                var thrifted = binaryAnnotation.ToThrift();
                var ep = thrifted.__isset.host ? thrifted.Host : span.Endpoint.ToThrift();
                ep.ServiceName = span.ServiceName;
                thrifted.Host = ep;
                s.BinaryAnnotations.Add(thrifted);
            }

            return s;
        }

        public static Endpoint ToThrift(this IPEndPoint endpoint)
        {
            return new Endpoint(BitConverter.ToInt32(endpoint.Address.GetAddressBytes(), 0), (short)endpoint.Port, string.Empty);
        }
    }
}