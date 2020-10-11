using System.Runtime.InteropServices;
using Logary.Ingestion.HTTP2;
using Microsoft.AspNetCore.Hosting;
using Microsoft.AspNetCore.Server.Kestrel.Core;
using Microsoft.Extensions.Hosting;

namespace Logary.Ingestion.gRPC
{
    public class Program
    {
        public static void Main(string[] args)
        {
            CreateHostBuilder(args).Build().Run();
        }

        public static IHostBuilder CreateHostBuilder(string[] args)
        {
            return Host.CreateDefaultBuilder(args)
                .ConfigureWebHostDefaults(webBuilder =>
                {
                    if (RuntimeInformation.IsOSPlatform(OSPlatform.OSX))
                        webBuilder.ConfigureKestrel(options =>
                        {
                            // Setup a HTTP/2 endpoint without TLS.
                            options.ListenLocalhost(5000, o => o.Protocols =
                                HttpProtocols.Http2);
                        });
                    webBuilder.UseStartup<Startup>();
                });
        }
    }
}