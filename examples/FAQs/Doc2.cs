type Message =
  [<Extension>]
  static member LogWithAck (logger, message, bufferCt, promiseCt) : Task<Task> =
    Alt.toTasks bufferCt promiseCt (Logger.logWithAck logger message)