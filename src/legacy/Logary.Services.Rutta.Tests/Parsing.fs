module Logary.Services.Rutta.Tests.Parsing

open Expecto
open Logary.Services.Rutta
open Logary.Services.Rutta.Program

[<Tests>]
let commandLineParsing =
  let makesValid state next =
    let subject = detailedParse state next

    match subject with
    | Choice1Of3 _ ->
      ()

    | other ->
      Tests.failtestf "Unexpected %A" other

  let makesInvalid state next =
    let subject = detailedParse state next

    match subject with
    | Choice1Of3 res ->
      Tests.failtestf "Didn't expect parse to yield valid result %A" res

    | other ->
      ()

  testList "parse" [
    testList "shipper" [
      testCase "state collection, Push_To" <| fun _ ->
        let state = Choice2Of3 []
        let arg = Push_To "tcp://127.0.0.1:5555"
        makesValid state arg

      testCase "state collection, Pub_To" <| fun _ ->
        let state = Choice2Of3 []
        let arg = Pub_To "tcp://127.0.0.1:5555"
        makesValid state arg

      testCase "state collection, no further args" <| fun _ ->
        let arg = Push_To "tcp://127.0.0.1:5555"
        let state = detailedParse (Choice2Of3 []) arg
        makesValid state (Health ("0.0.0.0", 8080))
    ]

    testList "router" [
      testCase "state collection, Router" <| fun _ ->
        let state = Choice2Of3 []
        let arg = Router "tcp://172.25.2.223:5556"
        makesValid state arg

      testCase "stateCollection, Router_Sub" <| fun _ ->
        let state = Choice2Of3 []
        let arg = Router_Sub "tcp://172.25.2.223:5556"
        makesValid state arg
    ]

    testList "proxy" [
      testCase "state collection, Proxy" <| fun _ ->
        let state = Choice2Of3 []
        let arg = Proxy ("tcp://10.42.0.10:5555", "172.25.2.223:5556")
        makesValid state arg
    ]

    testList "health" [
      testCase "state valid, Health -> state valid" <| fun _ ->
        let arg = Router_Sub "tcp://172.25.2.223:5556"
        let state = detailedParse (Choice2Of3 []) arg
        let arg' =  (Health ("0.0.0.0", 8080))
        makesValid state arg'
    ]

    testList "errors" [
      testCase "no mode" <| fun _ ->
        makesInvalid (Choice2Of3 []) (Health ("0.0.0.0", 8080))

      testCase "double mode" <| fun _ ->
        let arg = Router_Sub "tcp://172.25.2.223:5556"
        let state = Choice2Of3 []
        let state' = detailedParse state arg
        makesInvalid state' (Router "tcp://172.25.2.223:5556")
    ]
  ]
