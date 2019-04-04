module Program

open System
open Expecto
open Logary
open Logary.Targets
open Logary.Tests

let target =
  ElmahIO.create { logId = Guid.Parse "4e4dee38-cfbe-43db-921b-69d1d6654e5b"; apiKey = "3e3b081be7c14bfdbddef052836ae55b" }

let exnMsg =
  Message.event Error "Example message with exception"
  |> withException Message.addExn

[<Tests>]
let tests =
  testList "elmah.io tests" [
    TargetBaseline.basicTests "elmah.io" target true

    testList "getType" [
      testCase "of non-exception message" <| fun _ ->
        let msg = Message.event Info "User signed up" |> Message.setNameStr "A.B"
        let typ = ElmahIO.Impl.getType msg
        Expect.equal typ "A.B" "Should have name of Message as type"

      testCase "of message with exception" <| fun _ ->
        let typ = ElmahIO.Impl.getType exnMsg
        Expect.equal typ "System.Exception" "Should have exception type as type"

      testCase "formatting message captures exception details" <| fun _ ->
        let str = ElmahIO.Impl.format exnMsg
        Expect.stringContains str "middleWay" "Should contain parts of StackTrace."
    ]
  ]

[<EntryPoint>]
let main argv =
  Tests.runTestsInAssembly defaultConfig argv