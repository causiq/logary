using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Hosting;
using Microsoft.AspNetCore.HttpsPolicy;
using Microsoft.AspNetCore.Mvc;
using Microsoft.Extensions.Configuration;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Logging;
using Microsoft.Extensions.Options;
using Logary.AspNetCore;
using Logary.Configuration;
using Logary;

namespace AspNetCore.CSharp
{
  public class Startup
  {
    public Startup(IConfiguration configuration)
    {
      Configuration = configuration;
    }

    public IConfiguration Configuration { get; }

    // This method gets called by the runtime. Use this method to add services to the container.
    public void ConfigureServices(IServiceCollection services)
    {
      var logary = LogaryFactory.New("Logary.AspNetCore","localhost",
                with => with.InternalLogger(Logary.Configuration.ILogger.NewConsole(Logary.LogLevel.Debug))
                        .Target<Logary.Targets.LiterateConsole.Builder>("console")).Result;

      var logger = Logary.Log.Create<Startup> ();

      Logary.LoggerModule.logSimple(logger, Logary.MessageEx.EventFormat(Logary.LogLevel.Info,"does it {work}", "yes!"));

      services.AddLogging(x => x.ClearProviders().AddLogary(logary,true));
      services.AddMvc().SetCompatibilityVersion(CompatibilityVersion.Version_2_1);
    }

    // This method gets called by the runtime. Use this method to configure the HTTP request pipeline.
    public void Configure(IApplicationBuilder app, IHostingEnvironment env)
    {
      if (env.IsDevelopment())
      {
        app.UseDeveloperExceptionPage();
      }
      else
      {
        app.UseHsts();
      }

      app.UseMvc();
    }
  }
}