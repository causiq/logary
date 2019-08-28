module Logary.Tests.HashMap

open Logary
open Logary.Internals
open Expecto

let tests = [
  testCase "empty and add element" <| fun () ->
    HashMap.empty |> HashMap.add 2 4 |> ignore

  testCase "add" <| fun _ ->
    HashMap.empty |> HashMap.add "a" 24 |> ignore

  testCase "add + remove" <| fun _ ->
    let subject = HashMap.empty |> HashMap.add "a" 24 |> HashMap.remove "a"
    Expect.isFalse (subject |> HashMap.containsKey "a") "Should not contain 'a'"

  testCase "add x2" <| fun _ ->
    let subject = HashMap.empty |> HashMap.add "a" 2 |> HashMap.add "b" 3
    Expect.isTrue (subject |> HashMap.containsKey "a") "Should have 'a' key"
    Expect.isTrue (subject |> HashMap.containsKey "b") "Should have 'b' key"
    let subject = subject |> HashMap.remove "b"
    Expect.isTrue (subject |> HashMap.containsKey "a") "Should have 'a' key"
    Expect.isFalse (subject |> HashMap.containsKey "b") "Should have 'b' key"

  testPropertyWithConfig fsc "generate" <| fun (value: HashMap<string, int>) ->
    ignore value
    true
]
