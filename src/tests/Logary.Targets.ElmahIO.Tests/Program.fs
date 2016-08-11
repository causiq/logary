module Program

open System
open Fuchu
open Hopac
open ExpectoPatronum
open Logary
open Logary.Targets
open Logary.Tests.Targets
open Logary.Tests

let target =
  ElmahIO.create { logId = envForce "ELMAH_IO_LOG_ID" Guid.Parse }


[<Tests>]
let tests =
  testList "elmah.io tests" [
    Targets.basicTests "elmah.io" target
    Targets.integrationTests "elmah.io" target

    testList "getType" [
      testCase "of non-exception message" <| fun _ ->
        let msg = Message.event Info "User signed up" |> Message.setSimpleName "A.B"
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
  Tests.defaultMainThisAssembly argv