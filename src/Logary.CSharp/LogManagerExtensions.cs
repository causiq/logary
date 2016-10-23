using System.Threading.Tasks;
using NodaTime;

namespace Logary
{
    /// <summary>
    /// C# API for LogManager
    /// </summary>
    public static class LogManagerExtensions
    {
        /// <summary>
        /// Get a logger by name
        /// </summary>
        /// <returns>The logger instance for the given name</returns>
        /// <param name="logManager">Log manager.</param>
        /// <param name="name">The path/name that this logger will attach to the Messages.</param>
        public static Logger GetLogger(this LogManager logManager, string name)
        {
            return logManager.getLogger(PointNameModule.Parse(name));
        }

        /// <summary>
        /// Flush the pending messages. Will wait for the duration specified and then return. It will return ahead
        /// of time if all messages are flushed.
        /// 
        /// During normal operation this call should be 'fairly' quick, but if the targets are blocked it may time
        /// some serious amount of time.
        /// </summary>
        public static Task FlushPending(this LogManager logManager, Duration waitTime)
        {
            return logManager.flushPending(waitTime).ToTask();
        }
    }
}