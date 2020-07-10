module Logary.Tests.Targets.TextWriter

open Expecto
open Expecto.Flip
open Logary
open Hopac
open Hopac.Infixes
open Logary.Tests

[<Tests>]
let tests =
  testList "text writer" [
    TargetBaseline.basicTests "text writer" (fun name ->
      let _, twTargetConf = buildTextWriterTarget name
      twTargetConf) false

    testList "text writer prints" [
      testCaseJob "message" <| (job {
        let out, conf = buildTextWriterTarget "writing console target"
        let! ri = emptyRuntime
        let! targetApi = Target.create ri conf

        do! logMsgWaitAndShutdown targetApi (fun logAndWait ->
          logAndWait (Model.Event("Hello World!", None, level=Info)) >>- fun _ ->
          out.ToString()
            |> Expect.stringContains "logging with info level and then finalising the target" "Hello World!")
      })

      testCaseJob "fields are written out" <| (job {
        let out, conf = buildTextWriterTarget "writing console target"
        let! ri = emptyRuntime
        let! targetApi = Target.create ri conf

        do! logMsgWaitAndShutdown targetApi (fun logAndWait ->
          let message =
            let e = Model.Event("textwriter-test-init", None, level=Info)
            e.setFieldValues(Map [ "foo", "bar" ])
            e

          logAndWait message >>- fun _ ->
          for expected in [ "textwriter-test-init"; "foo"; "bar" ] do
            out.ToString()
              |> Expect.stringContains "logging with fields then finalising the target" expected)
      })

      testCaseJob "to correct stream" <| (job {
        let out, conf = buildTextWriterTarget "writing console target"
        let! ri = emptyRuntime
        let! targetApi = Target.create ri conf

        do! logMsgWaitAndShutdown targetApi (fun logAndWait ->
          logAndWait (Model.Event("Error line", None, level=Error)) >>= fun _ ->
          logAndWait (Model.Event("Fatal line", None, level=Fatal)) >>- fun _ ->
          out.ToString()
            |> Expect.stringContains "logging 'Error line' and 'Fatal line' to the target" "Error line"
          out.ToString()
            |> Expect.stringContains "logging 'Error line' and 'Fatal line' to the target" "Fatal line")
      })
    ]
  ]
  |> testLabel "targets"
  |> testLabel "logary"

