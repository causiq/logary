# Logary.Adapters.AspNetCore

[![Travis build status](https://travis-ci.org/logary/AspNetCore.png)](https://travis-ci.org/logary/AspNetCore)
[![AppVeyor Build status](https://ci.appveyor.com/api/projects/status/2i50hps9aipbwimb?svg=true)](https://ci.appveyor.com/project/lust4life/aspnetcore)
[![NuGet Badge](https://buildstats.info/nuget/Logary.Adapters.AspNetCore)](https://www.nuget.org/packages/Logary.Adapters.AspNetCore)


Logary logging for ASP.NET Core. This package routes ASP.NET Core log messages through Logary, so you can get information about ASP.NET's internal operations logged to the same Logary targets as your application events.

### Instructions


```csharp
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
```

More detail see [sample and run](https://github.com/logary/AspNetCore/blob/master/samples/AspNetCore.CSharp/Program.cs) :

default output （aspnet core console）：

![image](https://user-images.githubusercontent.com/3074328/38345936-7d3f8da6-38c4-11e8-9ac4-2b7dcff6fa06.png)

with logary (literate console => expand tokenise):

![image](https://user-images.githubusercontent.com/3074328/50156312-75f39f00-0309-11e9-9c96-77a40af15cb3.png)


with logary (literate console => oneline tokenise):

<img width="1876" alt="image" src="https://user-images.githubusercontent.com/3074328/50156442-d2ef5500-0309-11e9-876d-ab0ac73eb5ed.png">
