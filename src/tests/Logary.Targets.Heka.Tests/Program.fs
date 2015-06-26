module Program

open Fuchu
open System
open System.IO
open Logary
open Logary.Heka
open Logary.Heka.Messages
open Logary.Heka.Client
open Logary.Targets.Heka

[<Tests>]
let encoders =
  let emptyConf = HekaConfig.Empty
  let encode conf msg =
    use ms = new MemoryStream()
    match Encoder.encode conf ms msg with
    | Choice1Of2 promise -> Async.RunSynchronously promise
    | Choice2Of2 err -> Tests.failtestf "error: %A" err
    ms.ToArray()

  let givenSigned signerName payload =
    let sign = MessageSigningConfig.Create(signerName, key = "", hashAlgo = HmacHashFunction.MD5)
    let conf = { emptyConf with signingConfig = Some sign }
    conf, Message(``type`` = "TEST", timestamp = 1416840893000000000L, payload = payload)

  testList "protobuf encoding" [
    testCase "encode message stream" <| fun _ ->
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
      Assert.Equal("should contain same data", expected, encode emptyConf msg)

    testCase "encode message stream signed" <| fun _ ->
      let conf, msg = givenSigned "test" null
      let expected =
        [| 0x1e; 0x1c;
           // header:
           0x8; 0x10; // hdr len, msg len
           0x22;
           0x4; // string len = 4
            // string: "test"
            0x74; 0x65; 0x73; 0x74;

           0x28; 0x0;
           0x32; 0x10; 0x78; 0x35; 0x7e; 0x36; 0x35; 0x38; 0x3f; 0x9f; 0xbc;
           0x75; 0x98; 0x46; 0x1b; 0x5; 0xef; 0x2d;
           // separator
           0x1f;
           // message:
           0x10; 0x80; 0xc4; 0x8c; 0x94; 0x91; 0xa9; 0xe8; 0xd4; 0x13; 0x1a;
           0x4; 0x54; 0x45; 0x53; 0x54
        |] |> Array.map byte
      Assert.Equal("header should contain extra data", expected, encode conf msg)

    testCase "too large header" <| fun _ ->
      let conf, msg = givenSigned (String.replicate (int Constants.MaxHeaderSize) "x") null
      use ms = new MemoryStream()
      match Encoder.encode conf ms msg with
      | Choice1Of2 promise ->
        Async.RunSynchronously promise
        Tests.failtest "should have returned failure due to large header"
      | Choice2Of2 (MessageTooLarge err) ->
        Tests.failtest "unexpected return value"
      | Choice2Of2 (HeaderTooLarge err) ->
        Assert.Equal("error msg", "Message header too big, requires 280 (MAX_HEADER_SIZE = 255)",
                     err)

    testCase "too large message" <| fun _ ->
      let msg = Message(``type`` = "TEST", timestamp = 1416840893000000000L,
                        payload = String.replicate (int emptyConf.maxMessageSize) "x")
      use ms = new MemoryStream()
      match Encoder.encode emptyConf ms msg with
      | Choice1Of2 promise ->
        Async.RunSynchronously promise
        Tests.failtest "should have returned failure due to large header"
      | Choice2Of2 (HeaderTooLarge err) ->
        Tests.failtest "unexpected return value"
      | Choice2Of2 (MessageTooLarge err) ->
        Assert.Equal("error msg", "Message too big, requires 65556 (MAX_MESSAGE_SIZE = 65536)",
                     err)
  ]

[<Tests>]
let transformToMessage =
  testList "converting a log line to a Message" [
    testCase "just message" <| fun _ ->
      let ll = LogLine.create' LogLevel.Info "hello world"
      Assert.Equal("should eq msg", Message(), ll |> Message.ofLogLine)
  ]

[<EntryPoint>]
let main argv = Tests.defaultMainThisAssembly argv