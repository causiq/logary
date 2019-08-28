module Logary.Tests.Logger

open Expecto
open Expecto.Flip
open NodaTime
open System
open Logary

let tests = [
  testList "LoggerScope" [
    testCase "public interface :> Logger" <| fun () ->
      let hasLogger = typeof<Logger>.IsAssignableFrom(typeof<LoggerScope>)
      hasLogger
        |> Expect.isTrue "Should implement Logger"

    testCase "public interface :> IDisposable" <| fun () ->
      let hasIDisposable = typeof<IDisposable>.IsAssignableFrom(typeof<LoggerScope>)
      hasIDisposable
        |> Expect.isTrue "Should implement IDisposable"
  ]

  testList "TimeLogger" [
    testCase "public interface" <| fun () ->
      { new TimeLogger with
          member x.name: PointName = PointName.ofSingle "B"
          member x.logWithAck (waitForBuffers, level) (messageFactory: LogLevel -> Message) =
            LogResult.success
          member x.level: LogLevel = LogLevel.Info
          member x.Dispose () = ()
          member x.elapsed = Duration.Zero
          member x.bisect (label: string): unit =
            ()
          member x.finish (transform: Message -> Message) = ()
      }
      |> ignore
  ]

]
