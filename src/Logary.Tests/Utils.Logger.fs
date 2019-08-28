namespace Logary.Tests

open System.Collections.Generic
open Hopac
open Logary

type LogRecord =
  { waitForBuffers: bool
    level: LogLevel
    messageFactory: LogLevel -> Message
  }

  member x.getMessage () = x.messageFactory x.level

type StubLogger(?name: string, ?logLevel: LogLevel) =
  let sem = obj ()
  let logLevel = defaultArg logLevel Verbose
  let name = PointName.parse (defaultArg name "Logary.Tests.StubLogger")
  let _logged = ResizeArray<_>(50)

  static let success = Alt.always (Result.Ok Promise.unit)

  member x.logged = _logged :> IReadOnlyList<_>

  interface Logger with
    member x.logWithAck (waitForBuffers, level) messageFactory =
      lock sem <| fun () ->
      let factory = messageFactory >> Logger.ensureName name
      _logged.Add { waitForBuffers = waitForBuffers; level = level; messageFactory = factory }
      success
    member x.level = logLevel
    member x.name = name

module StubLogger =
  let create () = new StubLogger()
  let createWith name logLevel = new StubLogger(name, logLevel)
