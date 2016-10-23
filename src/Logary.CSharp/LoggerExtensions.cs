using System;
using System.Threading;
using System.Threading.Tasks;

namespace Logary
{
    /// <summary>
    /// Extensions to the Logger interface that were hard to do inside Logary proper
    /// because of having an existing Logger interface in the same compilation unit.
    /// </summary>
    public static class LoggerExtensions
    {
        /// <summary>
        /// A function that removes the need to pass CancellationTokens around.
        /// </summary>
        /// <returns>A task that yields after message-placed-in-buffer; a task
        /// that yields when message-flushed.</returns>
        /// <param name="logger">The logger to invoke the extension method on.</param>
        /// <param name="message">The message to log.</param>
        public static Task<Task> LogWithAck(this Logger logger, Message message)
        {
            CancellationToken bufferCt = default(CancellationToken);
            CancellationToken promiseCt = default(CancellationToken);
            return logger.LogWithAck(message, bufferCt, promiseCt);
        }

        /// <summary>See <see cref="LoggerModule.timeWithAckT{input,res}"/></summary>
        public static Func<Task<Task>> TimeWithAck(
            this Logger logger,
            Action action,
            string nameEnding = null,
            Func<Message, Message> transform = null)
        {
            var bufferCt = default(CancellationToken);
            var promiseCt = default(CancellationToken);
            return logger.TimeWithAck(action, bufferCt, promiseCt, nameEnding, transform);
        }

        /// <summary>See <see cref="LoggerModule.timeWithAckT{input,res}"/></summary>
        public static Func<Tuple<TRes, Task<Task>>> TimeWithAck<TRes>(
            this Logger logger,
            Func<TRes> func,
            string nameEnding = null,
            Func<Message, Message> transform = null)
        {
            var bufferCt = default(CancellationToken);
            var promiseCt = default(CancellationToken);
            return logger.TimeWithAck(func, bufferCt, promiseCt, nameEnding, transform);
        }

        /// <summary>See <see cref="LoggerModule.timeWithAckT{input,res}"/></summary>
        public static Func<TIn, Tuple<TRes, Task<Task>>> TimeWithAck<TIn, TRes>(
            this Logger logger,
            Func<TIn, TRes> func,
            string nameEnding = null,
            Func<Message, Message> transform = null)
        {
            var bufferCt = default(CancellationToken);
            var promiseCt = default(CancellationToken);
            return logger.TimeWithAck(func, bufferCt, promiseCt, nameEnding, transform);
        }
    }
}