module Logary.Tests.Targets.TextWriter

open Expecto
open Logary
open Hopac
open Hopac.Infixes
open Logary.Tests

[<Tests>]
let tests =
  testList "text writer" [
    TargetBaseline.basicTests "text writer" (fun name ->
      let _, _, twTargetConf = buildTextWriterTarget name
      twTargetConf) false

    testList "text writer prints" [
      testCaseJob "message" <| (job {
        let out, _, conf = buildTextWriterTarget "writing console target"
        let! ri = emptyRuntime
        let! targetApi = Target.create ri conf

        do! logMsgWaitAndShutdown targetApi (fun logAndWait ->
          logAndWait (Model.Event("Hello World!", None, level=Info)) >>- fun _ ->
          Expect.stringContains (string out) "Hello World!" "logging with info level and then finalising the target")
      })

      testCaseJob "fields" <| (job {
        let out, _, conf = buildTextWriterTarget "writing console target"
        let! ri = emptyRuntime
        let! targetApi = Target.create ri conf

        do! logMsgWaitAndShutdown targetApi (fun logAndWait ->
          let x = dict ["foo", "bar"]
          let message =
            let e = Model.Event("textwriter-test-init", None, level=Info)
            e.setFieldValues(Map [ "foo", "bar" ])
            e

          logAndWait message >>- fun _ ->
          Expect.stringContains (string out) "textwriter-test-init" "logging with fields then finalising the target"
          Expect.stringContains (string out) "foo" "logging with fields then finalising the target"
          Expect.stringContains (string out) "bar" "logging with fields then finalising the target")
      })

      testCaseJob "to correct stream" <| (job {
        let _, error, conf = buildTextWriterTarget "writing console target"
        let! ri = emptyRuntime
        let! targetApi = Target.create ri conf

        do! logMsgWaitAndShutdown targetApi (fun logAndWait ->
          logAndWait (Model.Event("Error line", None, level=Error)) >>= fun _ ->
          logAndWait (Model.Event("Fatal line", None, level=Fatal)) >>- fun _ ->
          let errorStr = string error
          Expect.stringContains errorStr "Error line" "logging 'Error line' and 'Fatal line' to the target"
          Expect.stringContains errorStr "Fatal line" "logging 'Error line' and 'Fatal line' to the target")
      })
    ]
  ]
  |> testLabel "targets"
  |> testLabel "logary"

