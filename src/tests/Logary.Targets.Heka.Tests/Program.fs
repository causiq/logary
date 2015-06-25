module Program

open Fuchu
open System
open System.IO
open Logary.Heka
open Logary.Heka.Messages
open Logary.Heka.Client

[<Tests>]
let encoders =
  testList "protobuf encoding" [
    testCase "encode message stream" <| fun _ ->
      let conf = HekaConfig.Empty

      let msg = Message(``type`` = "TEST", timestamp = 1416840893000000000L)

      let expected =
        [| 0x1e // record separator
           0x2  // header len
           0x8; 0x10; // protobuf header (msg len = 16)
           0x1f // unit separator
           // protobuf message:
           0x10; 0x80; 0xc4; 0x8c; 0x94; 0x91; 0xa9; 0xe8; 0xd4; 0x13; 0x1a;
           0x4; 0x54; 0x45; 0x53; 0x54
        |] |> Array.map byte

      let actual =
        use ms = new MemoryStream()
        match Encoder.encode conf ms msg with
        | Choice1Of2 promise -> Async.RunSynchronously promise
        | Choice2Of2 err -> Tests.failtestf "error: %A" err
        ms.ToArray()

      Assert.Equal("should contain same data", expected, actual)
  ]

[<EntryPoint>]
let main argv = Tests.defaultMainThisAssembly argv