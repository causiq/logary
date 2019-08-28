module Logary.Tests.Config

open Logary.Configuration
open Expecto

let tests = [
  testCase "create" <| fun () ->
    Config.create "tests" "hostname-123" |> ignore
]