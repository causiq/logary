module Logary.Tests.PointName

open Logary
open Expecto

[<Tests>]
let tests =
  testList "point name" [
    testCase "empty" <| fun () ->
      Expect.equal PointName.empty (PointName [||]) "Should be empty"

    testCase "c'tor" <| fun () ->
      PointName [| "a"; "b" |] |> ignore

    testCase "setEnding" <| fun () ->
      let pn = PointName.ofSingle "A" |> PointName.setEnding "B"
      Expect.equal pn (PointName.parse "A.B") "Should be equal to A.B.C"

    testCase "setEnding with dots – no parse" <| fun () ->
      let pn = PointName.ofSingle "A" |> PointName.setEnding "B.C"
      Expect.equal pn (PointName [| "A"; "B.C" |]) "Should be equal"

    testPropertyWithConfig fsc "isEmpty is callable for all" <| fun (pn: PointName) ->
      pn.isEmpty |> ignore
      true

    testPropertyWithConfig fsc "ToString is callable for all" <| fun (pn: PointName) ->
      pn.ToString() |> ignore
      true
  ]
  |> testLabel "logary"