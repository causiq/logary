module Logary.Tests.Program

open System
open System.Globalization
open System.Threading
open Expecto
open Expecto.ExpectoFsCheck
open Logary
open Logary.Message
open Logary.Configuration


type Obj() =
  member x.PropA =
    45
  member x.PropB =
    raise (Exception ("Oh noes, no referential transparency here"))

[<Tests>]
let tests =
  testList "logary" [
    testList "Config" [
      testCase "create" <| fun () ->
        Config.create "tests" "hostname-123" |> ignore
    ]

    testList "Message" [
      testCase "event : LogLevel -> string -> Message" <| fun _ ->
        let m = event Info "Hello world"
        Expect.equal m.level Info "Should have info level"
        Expect.equal m.value (Event "Hello world") "Should have template"

      testCase "eventX : string -> LogLevel -> Message" <| fun _ ->
        let m = eventX "Hello world" Info
        Expect.equal m.level Info "Should have info level"
        Expect.equal m.value (Event "Hello world") "Should have template"

      testCase "setFieldsFromObject : obj -> Message -> Message" <| fun () ->
        Tests.skiptest "Awaiting usage of TypeShape"
        let m = eventX "Hello world" Info |> setFieldsFromObject (Obj())
        let field = m.fields |> HashMap.tryFind (PointName.ofSingle "PropA") |> Option.get
        Expect.equal field (Field (Int64 45L, None)) "Should have PropA"
    ]
  ]

[<EntryPoint>]
let main args =
  let enUS = CultureInfo "en-US"
  Thread.CurrentThread.CurrentCulture   <- enUS
  Thread.CurrentThread.CurrentUICulture <- enUS
  Tests.runTestsInAssembly defaultConfig args