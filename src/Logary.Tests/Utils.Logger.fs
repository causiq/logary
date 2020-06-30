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

  member x.waitForAtLeast(num, ?maxCount, ?withMatch): Job<bool> =
    let maxCount = defaultArg maxCount 10
    let withMatch = defaultArg withMatch (fun _ -> true)

    job {
      let mutable cont = true
      let mutable i = 1
      let mutable res = false
      while cont && i <= maxCount do
        i <- i + 1
        let matching = _logged.FindAll(fun record -> withMatch record)
        if matching.Count >= num then
          cont <- false
          res <- true
        else
          do! timeOutMillis (i * 200)
      return res
    }



  interface Logger with
    member x.logWithAck (waitForBuffers, message) =
      lock sem <| fun () ->
      let message = message.getAsBase()
      message.ensureName name
      _logged.Add { waitForBuffers = waitForBuffers; message=message}
      LogResult.success
    member x.level = logLevel
    member x.name = name

module StubLogger =
  let create () = StubLogger()
  let createWith name logLevel = StubLogger(name, logLevel)
