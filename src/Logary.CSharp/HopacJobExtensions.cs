﻿using System.Threading.Tasks;
using Hopac;

namespace Logary
{
    /// <summary>
    /// Extensions for using Hopac from C#
    /// </summary>
    public static class HopacJobExtensions
    {
        /// <summary>
        /// Create a new task from the given job, started on another synchronization
        /// context than the job is running on. This will `start` the job on Hopac's
        /// scheduler but the task won't return on that same thread.
        /// </summary>
        public static Task<T> ToTask<T>(this Job<T> job)
        {
            // ReSharper disable once RedundantNameQualifier
            return global::Hopac.JobEx.Job.ToTask(job);
        }
    }
}