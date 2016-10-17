module Logary.Tests.Middleware

open Fuchu
open Logary
open Hopac

module Assert = ExpectoPatronum.Expect

type LoggerNext =
  Message -> Alt<Promise<Message>>

[<Tests>]
let middleware =
  testList "middleware" [
    let now = Message.setUTCTicks System.DateTime.UtcNow.Ticks

    let setUser next msg =
      msg
      |> Message.setContext "user" "haf12345678"
      |> next

    let setService next msg =
      msg
      |> Message.setContext "service" "app-service 123.1245"
      |> next

    let composed = Middleware.compose [ setUser; setService ]

    yield testCase "single" <| fun _ ->
      let msg = Middleware.identity id (Message.event Info "User logged in" |> now)
      Assert.equal msg (Message.eventInfo "User logged in" |> now) "identity"

    yield testCase "compose ordering" <| fun _ ->
      let calls = ref []
      let m1 next msg = calls := !calls @ ["m1"] ; next msg
      let m2 next msg = calls := !calls @ ["m2"] ; next msg
      let composed = Middleware.compose [ m1; m2 ]
      let res = composed (Message.eventInfo "compose" |> now)
      Assert.equal res (Message.eventInfo "compose" |> now) "should pass through"
      Assert.equal !calls [ "m1"; "m2" ] "calls in order"

    yield testCase "deep enriching" (* sounds good, doens't it? *) <| fun _ ->
      let msg = Message.eventInfo "App started"
      Assert.equal msg.context Map.empty "no context"

      let expected =
        { msg with
           context =
             Map.ofList [
               "user", String "haf12345678"
               "service", String "app-service 123.1245"
             ] }

      Assert.equal (composed msg) expected "adds to context like it should"

    yield testCase "compose twice" <| fun _ ->
      let msg = Message.eventInfo "App unload"
      let composed' = setService composed

      let expected =
        { msg with
           context =
             Map.ofList [
               "user", String "haf12345678"
               "service", String "app-service 123.1245"
             ] }

      Assert.equal (composed' msg) expected "adds to context like it should"

  ]