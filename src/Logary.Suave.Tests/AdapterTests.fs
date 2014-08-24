module Logary.Suave.Tests.AdapterTests

open Logary
open Logary.Suave
open Suave

open Fuchu

let test_loggers (min_level : Log.LogLevel) (line_level : Log.LogLevel) (line : LogLine ref) =
  let stub = { new Logger with
                  member x.Log ll = line := ll
                  member x.Measure m = ()
                  member x.Level = Info
                  member x.Name = "test stub" }

  let subject = SuaveAdapter(stub) :> Suave.Log.Logger

  subject.Log line_level <| fun () ->
    { message       = "test"
      ``exception`` = None
      level         = line_level
      path          = "test"
      ts_utc_ticks  = 0L
      trace         = Log.TraceHeader.Empty }

[<Tests>]
let tests =
  testList "with levels" [
    testCase "logs nothing on Debug level" <| fun _ ->
      let line : LogLine ref = ref (LogLine.create'' "a.b.c" "empty")
      test_loggers Log.Info Log.Debug line
      Assert.Equal("should have 'empty' message", "empty", (!line).message)
    testCase "logs same on Info level" <| fun _ ->
      let line : LogLine ref = ref (LogLine.create'' "a.b.c" "empty")
      test_loggers Log.Info Log.Info line
      Assert.Equal("should have 'test' message", "test", (!line).message)
    testCase "logs same on Error level" <| fun _ ->
      let line : LogLine ref = ref (LogLine.create'' "a.b.c" "empty")
      test_loggers Log.Info Log.Error line
      Assert.Equal("should have 'test' message", "test", (!line).message)
    ]

[<EntryPoint>]
let main argv =
  defaultMainThisAssembly argv
