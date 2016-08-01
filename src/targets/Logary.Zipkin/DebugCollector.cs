//-----------------------------------------------------------------------
// <copyright file="DebugCollector.cs" company="Bazinga Technologies Inc.">
//     Copyright (C) 2016 Bazinga Technologies Inc.
// </copyright>
//-----------------------------------------------------------------------

using System.IO;
using System.Threading.Tasks;

namespace Logary.Zipkin
{
    /// <summary>
    /// Debug collector used for printing spans into provided output.
    /// </summary>
    public class DebugCollector : ISpanCollector
    {
        private readonly TextWriter _writer;
        public DebugCollector(TextWriter writer)
        {
            _writer = writer;
        }

        /// <summary>
        /// Creates a debug collector instance logging all data on the standard output.
        /// </summary>
        public DebugCollector() : this(System.Console.Out)
        {
        }

        public async Task CollectAsync(params Span[] spans)
        {
            foreach (var span in spans)
                _writer.WriteLine(span.ToString());

            _writer.Flush();
        }
    }
}