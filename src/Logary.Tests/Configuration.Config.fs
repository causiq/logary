module Logary.Tests.Config

open System
open Hopac
open Logary
open Logary.Configuration
open Logary.Configuration.Target
open Logary.Configuration.TargetConfig
open Logary.Model
open Expecto
open Expecto.Flip

type ConfigConf =
  { a: int
    b: int
    c: Map<string, string>
  }
  interface TargetConfWriter<ConfigConf> with
    member x.write (key, value, hasOwnField) =
      match key with
      | "a" ->
        { x with a=int value }
      | "b" ->
        { x with b=int value }
      | _ ->
        { x with c = x.c |> Map.add key value }

let empty = { a = 2; b=3; c=Map.empty }

[<CompiledName "Create">]
let create (_: ConfigConf) name = TargetConf.createSimple (fun _ -> Job.unit ()) name

[<Tests>]
let tests =
  let dc =
    DynamicConfig.create
      typeof<ConfigConf>
      "Logary.Tests.Config, Logary.Tests"
      (Type.GetType "Logary.Tests.Config, Logary.Tests")

  let fancyURI =
    Uri "custom://discovery-server:9022/topic-name?a=3&b=4&d=dog"

  testList "config" [
    testCase "create" <| fun () ->
      Resource.create("tests", "hostname-123")
        |> Config.create
        |> ignore

    testList "Uri" [
      testCase "create" <| fun () ->
        let configDefault = dc.getDefault ()
        let config = Uri.parseConfig dc.configType configDefault fancyURI :?> ConfigConf

        config.a
          |> Expect.equal "Has value" 3
        config.b
          |> Expect.equal "Has value" 4
        config.c.["d"]
          |> Expect.equal "Has the dynamic value as well" "dog"
    ]

    testList "TargetConfig" [
      testCase "create" <| fun () ->
        let uri = Uri "custom://discovery-server:9022/topic-name?a=3&b=4&d=dog"
        Expect.throws
          "Has no support for custom targets"
          (fun () -> TargetConfig.create uri |> ignore)

      testCase "createWithConfig" <| fun () ->
        let config = schemeToConfAndDefault |> Map.add "custom" dc
        let subject = createWithConfig config fancyURI
        subject.name
          |> Expect.equal "Has name 'custom'" "custom"
    ]
  ]
  |> testLabel "logary"