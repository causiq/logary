using System;
using Hopac;
using System.Threading.Tasks;
using NodaTime;
using Logary.Internals;

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

    /// <summary>
    /// Extensions for using Hopac from C#
    /// </summary>
    public static class HopacExtensions
    {
        /// <summary>
        /// Start the job, but don't await it. This will queue it for running on
        /// Hopac and will wait until it's completed.
        /// </summary>
        public static void Start(this Job<Microsoft.FSharp.Core.Unit> job)
        {
            global::Hopac.Hopac.start(job);
        }

        /// <summary>
        /// Create a new task from the given job, started on another synchronization
        /// context than the job is running on. This will `start` the job on Hopac's
        /// scheduler but the task won't return on that same thread.
        /// </summary>
        public static Task<T> ToTask<T>(this Job<T> job)
        {
            return global::Logary.Internals.CSharp.ToTask(job);
        }
    }

    /// <summary>
    /// PointValue extensions methods.
    /// </summary>
    public static class PointValueExtensions
    {
        /// <summary>
        /// Tries the get event metric value from the PointValue DU.
        /// </summary>
        /// <returns>The get event.</returns>
        /// <param name="pval">Message.</param>
        public static string TryGetEvent(this PointValue pval)
        {
            string template;
            return pval.TryGetEvent(out template) ? template : null;
        }

        /// <summary>
        /// Tries the get gauge metric value from the PointValue DU.
        /// </summary>
        /// <returns>The get gauge.</returns>
        /// <param name="pval">Message.</param>
        public static Tuple<Value, Units> TryGetGauge(this PointValue pval)
        {
            Tuple<Value, Units> val;
            return pval.TryGetGauge(out val) ? val : null;
        }

        /// <summary>
        /// Tries the get derived value from the PointValue DU.
        /// </summary>
        /// <returns>The get derived.</returns>
        /// <param name="pval">The value to extrace.</param>
        public static Tuple<Value, Units> TryGetDerived(this PointValue pval)
        {
            Tuple<Value, Units> val;
            return pval.TryGetDerived(out val) ? val : null;
        }
    }
}