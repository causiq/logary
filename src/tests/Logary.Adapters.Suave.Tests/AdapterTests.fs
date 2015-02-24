module Logary.Adapters.Suave.Tests.AdapterTests

open global.Suave
open global.Suave.Logging

open Logary

open Fuchu

let test_loggers (min_level : Logging.LogLevel) (line_level : Logging.LogLevel) (line : LogLine ref) =
  let stub = { new Logger with
                  member x.LogVerbose fl = x.Log (fl ())
                  member x.LogDebug fl = x.Log (fl ())
                  member x.Log ll = line := ll
                  member x.Measure m = ()
                  member x.Level = Info
                  member x.Name = "test stub" }

  let subject = SuaveAdapter(stub) :> Suave.Logging.Logger

  subject.Log line_level <| fun () ->
    { message       = "test"
      ``exception`` = None
      level         = line_level
      path          = "test"
      ts_utc_ticks  = 0L
      trace         = Logging.TraceHeader.empty }

[<Tests>]
let tests =
  testList "with levels" [
    testCase "logs nothing on Debug level" <| fun _ ->
      let line : LogLine ref = ref (LogLine.create'' "a.b.c" "empty")
      test_loggers Logging.LogLevel.Info Logging.LogLevel.Debug line
      Assert.Equal("should have 'empty' message", "empty", (!line).message)

    testCase "logs same on Info level" <| fun _ ->
      let line : LogLine ref = ref (LogLine.create'' "a.b.c" "empty")
      test_loggers Logging.LogLevel.Info Logging.LogLevel.Info line
      Assert.Equal("should have 'test' message", "test", (!line).message)

    testCase "logs same on Error level" <| fun _ ->
      let line : LogLine ref = ref (LogLine.create'' "a.b.c" "empty")
      test_loggers Logging.LogLevel.Info Logging.LogLevel.Error line
      Assert.Equal("should have 'test' message", "test", (!line).message)
    ]
