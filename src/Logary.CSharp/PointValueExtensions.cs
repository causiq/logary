using System;

namespace Logary
{
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
        /// <param name="pval">The value to extract.</param>
        public static Tuple<Value, Units> TryGetDerived(this PointValue pval)
        {
            Tuple<Value, Units> val;
            return pval.TryGetDerived(out val) ? val : null;
        }
    }
}