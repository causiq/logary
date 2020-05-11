var message = MessageModule.Event(LogLevel.Warn, "Here be dragons!");
// force the buffers of all configured targets to be flushed
await logger.LogWithAck(message);