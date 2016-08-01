//-----------------------------------------------------------------------
// <copyright file="HttpCollector.cs" company="Bazinga Technologies Inc.">
//     Copyright (C) 2016 Bazinga Technologies Inc.
// </copyright>
//-----------------------------------------------------------------------

using System;
using System.IO;
using System.Net;
using System.Threading.Tasks;
using Thrift.Protocol;
using Thrift.Transport;
using Logary.Zipkin.Thrift;

namespace Logary.Zipkin
{
    /// <summary>
    /// A collector sending all data to zipkin endpoint using HTTP protocol.
    /// Spans are encoded using Thrift format.
    /// </summary>
    public class HttpCollector : ISpanCollector
    {
        readonly Uri _url;

        public HttpCollector(Uri url)
        {
            if (!url.IsAbsoluteUri)
                throw new ArgumentException($"URI '{url}' should be an absolute URI path to zipkin server");

            if (url.PathAndQuery == "/")
                url = new Uri(url, "api/v1/spans");

            _url = url;
        }

        public HttpCollector() : this(new Uri("http://localhost:9411/"))
        {
        }

        public async Task CollectAsync(params Span[] spans)
        {
            var request = WebRequest.CreateHttp(_url);
            request.Method = "POST";
            request.ContentType = "application/x-thrift";

            using (var output = new MemoryStream())
            {
                ThriftSpanSerializer.WriteSpans(spans, output);
                output.Position = 0;
                request.ContentLength = output.Length;
                using (var stream = await request.GetRequestStreamAsync())
                {
                    await output.CopyToAsync(stream);
                    await stream.FlushAsync();
                }
            }

            using (var reply = (HttpWebResponse)await request.GetResponseAsync())
            {
                if (reply.StatusCode != HttpStatusCode.Accepted)
                    throw new ZipkinCollectorException($"Zipkin HTTP receiver responded with status code {reply.StatusCode}");
            }
        }
    }
}