module Logary.Tests.Global

open Expecto
open Expecto.Flip
open Logary
open Logary.Internals
open Logary.Internals.Global

[<Tests>]
let tests =
  testList "global" [
    let logger = getStaticLogger (PointName.parse "a.b.c")

    testCase "Flyweight level" <| fun _ ->
      logger.level
        |> Expect.equal "should equal Flyweight's Fatal" Fatal

    testCase "initialise config" <| fun () ->
      let given () =
        let getLogger' (name: PointName) = StubLogger(name.ToString(), Warn) :> Logger
        let newConfig = { defaultConfig with getLogger = getLogger'; getTimestamp = fun () -> 1L }
        Global.initialise newConfig

      given ()

      getTimestamp()
        |> Expect.equal "Both return the same" 1L // see "given" above

      logger.level
        |> Expect.equal "New StubLogger's Warn level" Warn
  ]
  |> testLabel "logary"

