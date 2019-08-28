module Logary.Tests.Middleware

open Expecto
open Expecto.Flip
open Logary
open Logary.Message

let tests = [
    testCase "signature" <| fun () ->
      let f (_ : (Message -> Message) -> Message -> Message) = ()
      f (Middleware.host "local")

    testPropertyWithConfig fsc "identity" <| fun (m: Message) ->
      let rm = ref (event Fatal "nope")
      let save m = rm := m; m
      let out = Middleware.identity save m
      out |> Expect.equal "identity property" m
      !rm |> Expect.equal "middleware was called" m
]