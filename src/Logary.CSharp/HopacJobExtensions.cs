using System.Threading.Tasks;
using Hopac;
using Logary.Internals;

namespace Logary
{
    /// <summary>
    /// Extensions for using Hopac from C#
    /// </summary>
    public static class HopacJobExtensions
    {
        /// <summary>
        /// Start the job, but don't await it. This will queue it for running on
        /// Hopac and will wait until it's completed.
        /// </summary>
        public static void Start(this Job<Microsoft.FSharp.Core.Unit> job)
        {
            Hopac.Hopac.start(job);
        }

        /// <summary>
        /// Create a new task from the given job, started on another synchronization
        /// context than the job is running on. This will `start` the job on Hopac's
        /// scheduler but the task won't return on that same thread.
        /// </summary>
        public static Task<T> ToTask<T>(this Job<T> job)
        {
            return CSharp.ToTask(job);
        }
    }
}