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
    public async Task<string> Get(string val)
    {
      _logger.LogInformation("test some log {order}", 0);
      
      // use scope with message template and args
      using (_logger.BeginScope("first scope {hello}", "world"))
      {
        _logger.LogWarning("another log message in scope, {order}", 1);
        
        // use scope with raw state , with no message template
        using (_logger.BeginScope(Guid.NewGuid()))
        {
          _logger.LogError("in guid scope");
          
          // try some async
          await Task.Delay(1000);
          
          _logger.LogError("delay some time then log, still in guid scope");
        }
        
        _logger.LogInformation("out of guid scope, still in hello scope");
      }
      
      _logger.LogCritical("test done, out of scope.");

      try
      {
        var bad = Guid.Parse(val);
      }
      catch (Exception e)
      {
        // test exception
        _logger.LogError(e, "test exception from testscope/get action");
      }

      return ":p";
    }
  }
}