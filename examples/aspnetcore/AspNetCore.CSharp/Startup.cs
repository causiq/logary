using System;
using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Hosting;
using Microsoft.Extensions.Configuration;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Logging;
using Microsoft.AspNetCore.Mvc.Filters;


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
      // your mvc bla bla bla
      services.AddMvc(o => o.Filters.Add<TestScopeActionFilter>());
    }


    // This method gets called by the runtime. Use this method to configure the HTTP request pipeline.
    public void Configure(IApplicationBuilder app, IHostingEnvironment env)
    {
      if (env.IsDevelopment())
      {
        app.UseDeveloperExceptionPage();
      }
      
      app.UseMvc();
    }
  }

  public class TestScopeActionFilter : IActionFilter
  {
    private readonly ILogger<TestScopeActionFilter> _logger;

    public TestScopeActionFilter(ILogger<TestScopeActionFilter> logger)
    {
      _logger = logger;
    }

    public void OnActionExecuting(ActionExecutingContext context)
    {
      _logger.LogInformation("before action executing");
      
      var actionScope = _logger.BeginScope("some scope data from global filter : {CorrelationId}", Guid.NewGuid());
      context?.HttpContext?.Items?.Add("actionScope", actionScope);
    }

    public void OnActionExecuted(ActionExecutedContext context)
    {
      _logger.LogInformation("on action executed, before scope dispose");
      
      (context?.HttpContext?.Items["actionScope"] as IDisposable)?.Dispose();
      
      _logger.LogInformation("on action executed, after scope dispose");
    }
  }
}