module Logary.Tests.LogLevel

open Expecto
open Logary

[<Tests>]
let tests =
  testList "log level" [
    testProperty "asInt" <| fun (level: LogLevel) ->
      level.asInt |> ignore

    testProperty "=" <| fun (level: LogLevel) ->
      level = level

    testProperty "<>" <| fun (level: LogLevel) ->
      if level = Info then level = Info else
      level <> Info
  ]
  |> testLabel "logary"