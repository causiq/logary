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

let innermost () =
  raise (Exception "Bad things going on")

let middleWay () =
  1 + 3 |> ignore
  innermost ()

let withException f =
  try
    middleWay ()
  with e ->
    f e

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
        let msg =
          Message.event Error "Unhandled exception"
          |> Message.setSimpleName "A.B.C"
          |> withException Message.addExn

        let typ = ElmahIO.Impl.getType msg

        Expect.equal typ "System.Exception" "Should have exception type as type"
    ]
  ]

[<EntryPoint>]
let main argv =
  Tests.defaultMainThisAssembly argv