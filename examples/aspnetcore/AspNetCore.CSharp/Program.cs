using System;
using Logary;
using Logary.Adapters.AspNetCore;
using Logary.Configuration;
using Logary.CSharp;
using LogaryHelper;
using Microsoft.AspNetCore;
using Microsoft.AspNetCore.Hosting;
using Microsoft.Extensions.Logging;
using LogLevel = Logary.LogLevel;

namespace AspNetCore.CSharp
{
  public class Program
  {
    public static void Main(string[] args)
    {
      var logary = ConfigLogary.create("localhost");
      var logger = Log.Create<Program>();

      logger.LogEventFormat(LogLevel.Verbose, "does it work? {truth}", "yes!"); // no need, only for example

      try
      {
        CreateWebHostBuilder(args, logary).Build().Run();
      }
      catch (Exception e)
      {
        logger.LogEvent(LogLevel.Fatal, "host terminated unexpectedly", e);
      }
      finally
      {
        // ensure flush and shutdown logary
        Hopac.Hopac.startAsTask(logary.shutdown()).ConfigureAwait(false).GetAwaiter().GetResult();
      }
    }

    public static IWebHostBuilder CreateWebHostBuilder(string[] args, LogManager logary) =>
      WebHost.CreateDefaultBuilder(args)
        .ConfigureLogging(logging =>
          logging.ClearProviders().AddLogary(logary) 
//          logging.ClearProviders().AddConsole(b=>b.IncludeScopes = true)
          // switch here to show difference
          )
        .UseStartup<Startup>();
  }
}