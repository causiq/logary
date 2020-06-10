namespace Logary

open Logary.Model

type internal LoggerWrapper(logger: Logger) =
  abstract name: PointName
  abstract level: LogLevel
  abstract logWithAck: waitForBuffers: bool * message: Logary.LogaryMessage -> LogResult

  default x.name = logger.name
  default x.level = logger.level
  default x.logWithAck(waitForBuffers: bool, message: Logary.LogaryMessage): LogResult =
    if message.name.isEmpty then match message with :? LogaryMessageBase as b -> b.ensureName x.name | _ -> ()
    logger.logWithAck (waitForBuffers, message)

  interface Logger with
    member x.name = x.name
    member x.level = x.level
    member x.logWithAck (waitForBuffers, message) = x.logWithAck(waitForBuffers, message)
