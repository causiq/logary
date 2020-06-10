namespace Logary.Tests

open System.Collections.Generic
open Logary

type LogRecord =
  { waitForBuffers: bool
    level: LogLevel
    message: LogaryMessage }

type StubLogger(?name: string, ?logLevel: LogLevel) =
  let sem = obj ()
  let logLevel = defaultArg logLevel Verbose
  let name = PointName.parse (defaultArg name "Logary.Tests.StubLogger")
  let _logged = ResizeArray<_>(50)

  member x.logged = _logged :> IReadOnlyList<_>

  interface Logger with
    member x.logWithAck (waitForBuffers, message) =
      lock sem <| fun () ->
      if message.name.isEmpty then message.name <- name
      _logged.Add { waitForBuffers = waitForBuffers; level = logLevel; message=message}
      LogResult.success
    member x.level = logLevel
    member x.name = name

module StubLogger =
  let create () = StubLogger()
  let createWith name logLevel = StubLogger(name, logLevel)
