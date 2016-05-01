using System;
using Hopac;
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
            return logManager.flushPending (waitTime).ToTask();
        }
    }

    /// <summary>
    /// Extensions for using Hopac from C#
    /// </summary>
    public static class HopacExtensions
    {
        /// <summary>
        /// Start the job
        /// </summary>
        /// <param name="job">Job to start.</param>
        public static void Start(this Job<Microsoft.FSharp.Core.Unit> job)
        {
            Hopac.Job.Global.start(job);
        }

        /// <summary>
        /// Convert the task to a job.
        /// </summary>
        /// <returns>The task.</returns>
        /// <param name="job">Job to convert to a task.</param>
        /// <typeparam name="T">The 1st type parameter.</typeparam>
        public static Task<T> ToTask<T>(this Job<T> job)
        {
            return Logary.Internals.CSharpFacade.ToTask(job);
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