module Logary.Tests.Message

open Fuchu
open Fuchu.FuchuFsCheck
open Logary.Utils.Chiron
open Logary

[<Tests>]
let tests =
  testList "Serialization" [
      testProperty "Serialization of message can round trip" <| fun (message : Message) ->
        message |> Json.serialize |> Json.format |> Json.parse
        |> Json.deserialize |> (=) message
    ]
