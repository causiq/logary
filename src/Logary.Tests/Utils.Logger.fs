namespace Logary.Tests

open System.Collections.Generic
open Hopac
open Logary
open Logary.Internals

type LogRecord = { waitForBuffers: bool; message: LogaryMessage }

type StubLogger(?name: string, ?logLevel: LogLevel) =
  let sem = obj ()
  let logLevel = defaultArg logLevel Verbose
  let name = PointName.parse (defaultArg name "Logary.Tests.StubLogger")
  let _logged = ResizeArray<_>(50)

  member x.logged = _logged :> IReadOnlyList<_>

  member x.waitForAtLeast(num, ?maxCount) =
    let maxCount = defaultArg maxCount 10

    job {
      let mutable cont = true
      let mutable i = 1
      while cont && i <= maxCount do
        i <- i + 1
        if _logged.Count >= num then
          cont <- false
        else
          do! timeOutMillis (i * 20)
    }



  interface Logger with
    member x.logWithAck (waitForBuffers, message) =
      lock sem <| fun () ->
      let message = message.getAsBase Model.Event
      message.ensureName name
      _logged.Add { waitForBuffers = waitForBuffers; message=message}
      LogResult.success
    member x.level = logLevel
    member x.name = name

module StubLogger =
  let create () = StubLogger()
  let createWith name logLevel = StubLogger(name, logLevel)
