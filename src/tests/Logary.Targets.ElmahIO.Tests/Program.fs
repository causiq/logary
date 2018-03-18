module Program

open System
open System.Runtime.CompilerServices
open Expecto
open Hopac
open Logary
open Logary.Targets
open Logary.Tests

let target =
  ElmahIO.create { logId = Guid.Parse "4e4dee38-cfbe-43db-921b-69d1d6654e5b"; apiKey = "3e3b081be7c14bfdbddef052836ae55b" }

[<MethodImpl(MethodImplOptions.NoInlining)>]
let innermost () =
  raise (Exception "Bad things going on")

[<MethodImpl(MethodImplOptions.NoInlining)>]
let middleWay () =
  1 + 3 |> ignore
  innermost ()

[<MethodImpl(MethodImplOptions.NoInlining)>]
let withException f =
  try
    middleWay ()
  with e ->
    f e


let exnMsg =
  Message.event Error "some ex occur"
  |> withException Message.addExn

[<Tests>]
let tests =
  testList "elmah.io tests" [
    Logary.Tests.CoreTargets.basicTests "elmah.io" target

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
        Expect.stringContains str "withException" "Should contain parts of StackTrace."
    ]
  ]

[<EntryPoint>]
let main argv =
  Tests.runTestsInAssembly defaultConfig argv