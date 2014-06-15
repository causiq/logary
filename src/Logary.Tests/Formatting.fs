module Logary.Tests.Formatting

open Fuchu
open Swensen.Unquote

open System
open System.IO

open NodaTime
open Newtonsoft.Json

open Logary
open Logary.Formatting
open Logary.Tests.TestDSL

let private sampleMessage =
  { message       = "this is bad"
    timestamp     = Instant.FromSecondsSinceUnixEpoch(3L).PlusTicks(1234567L)
    data          = Map.empty
    level         = LogLevel.Error
    tags          = ["error"; "bad"]
    path          = "a.b.c.d"
    ``exception`` = None }

[<Tests>]
let tests =
  testList "formatting" [
    testCase "StringFormatter.Verbatim" <| fun _ ->
      (because "formatting the message verbatim" <| fun _ ->
        Log.errorStr "hello world"
        |> StringFormatter.Verbatim.format)
      |> should equal "hello world"
      |> thatsIt

    testCase "StringFormatter.VerbatimNewline" <| fun _ ->
      (because "logging verbatim with newline" <| fun () ->
        Log.infoStr "hi there"
        |> StringFormatter.VerbatimNewline.format)
      |> should equal (sprintf "hi there%s" Environment.NewLine)
      |> thatsIt

    testCase "StringFormatter.LevelDatetimePathMessageNl no exception, no tags" <| fun _ ->
      (because "logging with LevelDatetimePathMessageNl" <| fun () ->
        { sampleMessage with tags = [] } |> StringFormatter.LevelDatetimePathMessageNl.format)
      |> should equal (
          sprintf "E 1970-01-01T00:00:03.1234567+00:00: this is bad [a.b.c.d]%s"
            Environment.NewLine)
      |> thatsIt

    testCase "StringFormatter.LevelDatetimePathMessageNl no exception, tags" <| fun _ ->
      (because "logging with LevelDatetimePathMessageNl" <| fun () ->
        sampleMessage |> StringFormatter.LevelDatetimePathMessageNl.format)
      |> should equal (
          sprintf "E 1970-01-01T00:00:03.1234567+00:00: this is bad [a.b.c.d] {error, bad}%s"
            Environment.NewLine)
      |> thatsIt

    testCase "StringFormatter.LevelDatetimePathMessageNl with exception, tags" <| fun _ ->
      let e = new Exception("Gremlings in the machinery")
      (because "logging with exception attached" <| fun () ->
        { sampleMessage with ``exception`` = e |> Some }
        |> StringFormatter.LevelDatetimePathMessageNl.format)
      |> should equal (
        sprintf "E 1970-01-01T00:00:03.1234567+00:00: this is bad [a.b.c.d] {error, bad} cont...%s%O%s"
          Environment.NewLine
          e
          Environment.NewLine)
      |> thatsIt

    testCase "``JsonFormatter has no newline characters``" <| fun _ ->
      (because "logging message with newline in it" <| fun () ->
          { sampleMessage with message = "here\n  we\ngo!" } |> JsonFormatter.Default().format)
      |> should equal ("""{"message":"here\n  we\ngo!","data":{},"level":"error","tags":["error","bad"],""" +
                      """"timestamp":"1970-01-01T00:00:03.1234567Z","path":"a.b.c.d"}""")
      |> thatsIt
    ]