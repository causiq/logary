using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using Microsoft.AspNetCore.Mvc;
using Microsoft.Extensions.Logging;

namespace AspNetCore.CSharp.Controllers
{
  [Route("api/[controller]")]
  [ApiController]
  public class TestScopeController : ControllerBase
  {
    ILogger<TestScopeController> _logger;

    public TestScopeController(ILogger<TestScopeController> logger)
    {
      _logger = logger;
    }


    // GET api/TestScope
    [HttpGet]
    public async Task<string> Get()
    {

      _logger.LogInformation("test some log {order}", 0);

      using (_logger.BeginScope("first scope {hello}", "world"))
      {
        _logger.LogWarning("another log message in scope, {order}", 1);
        using (_logger.BeginScope(Guid.NewGuid()))
        {
          _logger.LogError("wired beginscope guid logger api");
          await Task.Delay(1000);
          
          _logger.LogError("delay some time then log, still in scope, inner most");

        }
        
        _logger.LogInformation("delay some time then log, still in scope, inner");
      }
      
      _logger.LogCritical("test done, out of scope.");

      return ":p";
    }
  }
}