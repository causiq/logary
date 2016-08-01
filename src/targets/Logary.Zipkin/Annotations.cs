//-----------------------------------------------------------------------
// <copyright file="Annotations.cs" company="Bazinga Technologies Inc.">
//     Copyright (C) 2016 Bazinga Technologies Inc.
// </copyright>
//-----------------------------------------------------------------------

using System;
using System.Net;
using System.Text;

namespace Logary.Zipkin
{
    public static class Annotations
    {
        private static readonly byte[] TrueBinary = { 1 };
        private static readonly byte[] FalseBinary = { 0 };

        /// <summary>
        /// Optionally logs an attempt to send a message on the wire. Multiple wire send
        /// events could indicate network retries. A lag between client or server send
        /// and wire send might indicate queuing or processing delay.
        /// </summary>
        public static Annotation WireSend(DateTime timestamp) => new Annotation(AnnotationConstants.WireSend, timestamp);

        /// <summary>
        /// Optionally logs an attempt to receive a message from the wire. Multiple wire
        /// receive events could indicate network retries. A lag between wire receive
        /// and client or server receive might indicate queuing or processing delay.
        /// </summary>
        public static Annotation WireReceive(DateTime timestamp) => new Annotation(AnnotationConstants.WireReceive, timestamp);

        /// <summary>
        /// The client sent ("cs") a request to a server. There is only one send per
        /// span. For example, if there's a transport error, each attempt can be logged
        /// as a <see cref="WireSend"/> annotation.
        /// 
        /// If chunking is involved, each chunk could be logged as a separate
        /// <see cref="ClientSendFragment"/> in the same span.
        /// 
        /// Annotation.host is not the server. It is the host which logged the send
        /// event, almost always the client. When logging <see cref="ClientSend"/>, instrumentation
        /// should also log the <see cref="ServerAddress"/>.
        /// </summary>
        public static Annotation ClientSend(DateTime timestamp) => new Annotation(AnnotationConstants.ClientSend, timestamp);

        /// <summary>
        /// The client received ("cr") a response from a server. There is only one
        /// receive per span. For example, if duplicate responses were received, each
        /// can be logged as a <see cref="WireReceive"/> annotation.
        /// 
        /// If chunking is involved, each chunk could be logged as a separate
        /// <see cref="ClientReceiveFragment"/> in the same span.
        /// 
        /// Annotation.host is not the server. It is the host which logged the receive
        /// event, almost always the client. The actual endpoint of the server is
        /// recorded separately as <see cref="ServerAddress"/> when <see cref="ClientSend"/> is logged.
        /// </summary>
        public static Annotation ClientReceive(DateTime timestamp) => new Annotation(AnnotationConstants.ClientReceive, timestamp);
        
        /// <summary>
        /// The server sent ("ss") a response to a client. There is only one response
        /// per span. If there's a transport error, each attempt can be logged as a
        /// <see cref="WireSend"/> annotation.
        /// 
        /// Typically, a trace ends with a server send, so the last timestamp of a trace
        /// is often the timestamp of the root span's server send.
        /// 
        /// If chunking is involved, each chunk could be logged as a separate
        /// <see cref="ServerSendFragment"/> in the same span.
        /// 
        /// Annotation.host is not the client. It is the host which logged the send
        /// event, almost always the server. The actual endpoint of the client is
        /// recorded separately as <see cref="ClientAddress"/> when <see cref="ServerReceive"/> is logged.
        /// </summary>
        public static Annotation ServerSend(DateTime timestamp) => new Annotation(AnnotationConstants.ServerSend, timestamp);

        /// <summary>
        /// The server received ("sr") a request from a client. There is only one
        /// request per span.  For example, if duplicate responses were received, each
        /// can be logged as a <see cref="WireReceive"/> annotation.
        /// 
        /// Typically, a trace starts with a server receive, so the first timestamp of a
        /// trace is often the timestamp of the root span's server receive.
        /// 
        /// If chunking is involved, each chunk could be logged as a separate
        /// <see cref="ServerReceiveFragment"/> in the same span.
        /// 
        /// Annotation.host is not the client. It is the host which logged the receive
        /// event, almost always the server. When logging <see cref="ServerReceive"/>, instrumentation
        /// should also log the <see cref="ClientAddress"/>.
        /// </summary>
        public static Annotation ServerReceive(DateTime timestamp) => new Annotation(AnnotationConstants.ServerReceive, timestamp);
        
        /// <summary>
        /// Optionally logs progress of a (<see cref="ClientSend"/>, <see cref="WireSend"/>). For example, this
        /// could be one chunk in a chunked request.
        /// </summary>
        public static Annotation ClientSendFragment(DateTime timestamp) => new Annotation(AnnotationConstants.ClientSendFragment, timestamp);

        /// <summary>
        /// Optionally logs progress of a (<see cref="ClientReceive"/>, <see cref="WireReceive"/>). For example, this
        /// could be one chunk in a chunked response.
        /// </summary>
        public static Annotation ClientReceiveFragment(DateTime timestamp) => new Annotation(AnnotationConstants.ClientReceiveFragment, timestamp);

        /// <summary>
        /// Optionally logs progress of a (<see cref="ServerSend"/>, <see cref="WireSend"/>). For example, this
        /// could be one chunk in a chunked response.
        /// </summary>
        public static Annotation ServerSendFragment(DateTime timestamp) => new Annotation(AnnotationConstants.ServerSendFragment, timestamp);

        /// <summary>
        /// Optionally logs progress of a (<see cref="ServerReceive"/>, <see cref="WireReceive"/>). For example, this
        /// could be one chunk in a chunked request.
        /// </summary>
        public static Annotation ServerReceiveFragment(DateTime timestamp) => new Annotation(AnnotationConstants.ServerSendFragment, timestamp);

        /// <summary>
        /// Indicates a client address ("ca") in a span. Most likely, there's only one.
        /// Multiple addresses are possible when a client changes its ip or port within
        /// a span.
        /// </summary>
        public static BinaryAnnotation ClientAddress(IPEndPoint endpoint) => new BinaryAnnotation(AnnotationConstants.ClientAddress, TrueBinary, AnnotationType.Bool, endpoint);

        /// <summary>
        /// Indicates a server address ("sa") in a span. Most likely, there's only one.
        /// Multiple addresses are possible when a client is redirected, or fails to a
        /// different server ip or port.
        /// </summary>
        public static BinaryAnnotation ServerAddress(IPEndPoint endpoint) => new BinaryAnnotation(AnnotationConstants.ServerAddress, TrueBinary, AnnotationType.Bool, endpoint);

        /// <summary>
        /// Creates a <see cref="BinaryAnnotation"/> for a boolean value.
        /// </summary>
        public static BinaryAnnotation Binary(string key, bool value) => new BinaryAnnotation(key, value ? TrueBinary : FalseBinary, AnnotationType.Bool);
        
        /// <summary>
        /// Creates a <see cref="BinaryAnnotation"/> for a string integer.
        /// </summary>
        public static BinaryAnnotation Binary(string key, string value) => new BinaryAnnotation(key, Encoding.UTF8.GetBytes(value), AnnotationType.String);
    }
}