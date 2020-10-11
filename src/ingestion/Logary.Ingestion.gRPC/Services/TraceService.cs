using System.Threading.Tasks;
using Grpc.Core;
using Microsoft.Extensions.Logging;
using Opentelemetry.Proto.Collector.Trace.V1;

namespace Logary.Ingestion.gRPC.Services
{
    public class TraceService : Opentelemetry.Proto.Collector.Trace.V1.TraceService.TraceServiceBase
    {
        readonly ILogger<TraceService> _logger;

        public TraceService(ILogger<TraceService> logger)
        {
            _logger = logger;
        }

        public override Task<ExportTraceServiceResponse> Export(ExportTraceServiceRequest request, ServerCallContext context)
        {
            _logger.Log(Microsoft.Extensions.Logging.LogLevel.Information, $"Received call with resource spans count {request.ResourceSpans.Count}");

            return Task.FromResult(new ExportTraceServiceResponse());
        }
    }
}