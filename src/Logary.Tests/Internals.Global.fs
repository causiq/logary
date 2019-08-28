module Logary.Tests.Global

open Logary
open Logary.Message
open Logary.Internals
open Expecto

let tests = [
    testCase "initialise config" <| fun () ->
      let logger = Global.getStaticLogger (PointName.parse "a.b.c")
      let defaultSemaphore = Global.defaultConfig.getConsoleSemaphore ()
      Expect.equal (Global.getConsoleSemaphore ()) defaultSemaphore "should equal with no change"

      let mutable message = event LogLevel.Info "None"

      Expect.equal logger.level LogLevel.Fatal "should equal NullLogger's Fatal"

      eventX "test" |> logger.info
      Expect.equal message.value "None" "should equal with no effect"
      Expect.equal message.level LogLevel.Info "should equal with no effect"

      let getLogger' name = {
        new Logger with
          member x.name = name
          member x.level = Warn
          member x.logWithAck (waitForBuffer, level) factory =
            message <- factory level
            LogResult.success
      }

      let newConfig = { Global.defaultConfig with  getLogger = getLogger'; getTimestamp = fun () -> 1L }
      Global.initialise newConfig
      Expect.equal (Global.getTimestamp ()) (newConfig.getTimestamp ()) "should equal with new change"
      Expect.equal (Global.getConsoleSemaphore ()) defaultSemaphore "should equal with no change"

      eventX "test" |> logger.error
      Expect.equal message.value "test" "should equal with new effect"
      Expect.equal message.level LogLevel.Error "should equal with new effect"
      Expect.equal logger.level LogLevel.Warn "should equal new logger's Warn"
]

