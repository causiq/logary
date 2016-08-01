using System.IO;
using Thrift.Protocol;
using Thrift.Transport;

namespace Logary.Zipkin.Thrift
{
    /// <summary>
    /// Serializer able to serialize <see cref="Span"/> collections into thrift-encoded binaries.
    /// </summary>
    public static class ThriftSpanSerializer
    {
        /// <summary>
        /// Serializes array of spans using Thrift protocol.
        /// </summary>
        public static void WriteSpans(Zipkin.Span[] spans, Stream outputStream)
        {
            var transport = new TStreamTransport(null, outputStream);
            var protocol = new TBinaryProtocol(transport);

            protocol.WriteListBegin(new TList(TType.Struct, spans.Length));
            foreach (var span in spans)
            {
                var thrift = span.ToThrift();
                thrift.Write(protocol);
            }
            protocol.WriteListEnd();
            transport.Flush();

        }
    }
}