//-----------------------------------------------------------------------
// <copyright file="Span.cs" company="Bazinga Technologies Inc.">
//     Copyright (C) 2016 Bazinga Technologies Inc.
// </copyright>
//-----------------------------------------------------------------------

using System.Collections.Generic;
using System.Net;
using System.Text;

namespace Logary.Zipkin
{
    /// <summary>
    /// A set of <see cref="Annotation"/> and <see cref="BinaryAnnotation"/> elements that correspond to a particular RPC. 
    /// Spans contain identifying information such as traceId, spandId, parentId, and RPC name.
    /// </summary>
    public sealed class Span
    {
        /// <summary>
        /// Trace header containing are identifiers necessary to locate current span.
        /// </summary>
        public readonly TraceHeader TraceHeader;

        /// <summary>
        /// Name of the service displayed by Zipkin UI.
        /// </summary>
        public readonly string ServiceName;

        public readonly string Name;

        /// <summary>
        /// Collection of annotations recorder withing current span time frame.
        /// </summary>
        public readonly ICollection<Annotation> Annotations;

        /// <summary>
        /// Collection of binary annotations used to attach additional metadata with the span itself.
        /// </summary>
        public readonly ICollection<BinaryAnnotation> BinaryAnnotations;

        /// <summary>
        /// Endpoint, target span's service is listening on.
        /// </summary>
        public readonly IPEndPoint Endpoint;
        
        public Span(TraceHeader traceHeader, IPEndPoint endpoint, string serviceName = null, string name = null)
        {
            TraceHeader = traceHeader;
            ServiceName = serviceName ?? "Unknown";
            Name = name ?? "Unknown";
            Annotations = new List<Annotation>();
            BinaryAnnotations = new List<BinaryAnnotation>();
            Endpoint = endpoint;
        }

        public Span(TraceHeader traceHeader, IPEndPoint endpoint, ICollection<Annotation> annotations, ICollection<BinaryAnnotation> binaryAnnotations, string serviceName, string name)
        {
            TraceHeader = traceHeader;
            ServiceName = serviceName ?? "Unknown";
            Name = name ?? "Unknown";
            Annotations = annotations;
            BinaryAnnotations = binaryAnnotations;
            Endpoint = endpoint;
        }

        public Span(ulong traceId, ulong spanId, ulong? parentId = null)
            : this(new TraceHeader(traceId, spanId, parentId, true), new IPEndPoint(0, 0))
        {
        }

        /// <summary>
        /// Records an annotation within current span. 
        /// Also sets it's endpoint if it was not set previously.
        /// </summary>
        public void Record(Annotation annotation)
        {
            if (annotation.Endpoint == null)
            {
                annotation = annotation.WithEndpoint(Endpoint);
            }

            Annotations.Add(annotation);
        }

        /// <summary>
        /// Records a binary annotation within current span. 
        /// Also sets it's endpoint if it was not set previously.
        /// </summary>
        public void Record(BinaryAnnotation binaryAnnotation)
        {
            if (binaryAnnotation.Endpoint == null)
            {
                binaryAnnotation = binaryAnnotation.WithEndpoint(Endpoint);
            }

            BinaryAnnotations.Add(binaryAnnotation);
        }

        public override string ToString()
        {
            var sb = new StringBuilder()
                .Append("Span(service:").Append(ServiceName).Append(", name:").Append(Name)
                .Append(", trace:").Append(TraceHeader.ToString())
                .Append(", endpoint:").Append(Endpoint.ToString())
                .Append(", annotations:[");

            foreach (var annotation in Annotations)
            {
                sb.Append(annotation.ToString()).Append(' ');
            }

            if (BinaryAnnotations.Count > 0)
            {
                sb.Append("], binnaryAnnotations:[");

                foreach (var annotation in BinaryAnnotations)
                {
                    sb.Append(annotation.ToString()).Append(' ');
                }
            }

            sb.Append("])");

            return sb.ToString();
        }
    }
}