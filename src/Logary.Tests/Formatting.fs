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
        LogLine.error "hello world"
        |> StringFormatter.Verbatim.format)
      |> should equal "hello world"
      |> thatsIt

    testCase "StringFormatter.VerbatimNewline" <| fun _ ->
      (because "logging verbatim with newline" <| fun () ->
        LogLine.info "hi there"
        |> StringFormatter.VerbatimNewline.format)
      |> should equal (sprintf "hi there%s" Environment.NewLine)
      |> thatsIt

    testCase "StringFormatter.LevelDatetimePathMessageNl no exception, no tags" <| fun _ ->
      (because "logging with LevelDatetimePathMessageNl" <| fun () ->
        { sampleMessage with tags = [] } |> StringFormatter.LevelDatetimeMessagePathNl.format)
      |> should equal (
          sprintf "E 1970-01-01T00:00:03.1234567+00:00: this is bad [a.b.c.d]%s"
            Environment.NewLine)
      |> thatsIt

    testCase "StringFormatter.LevelDatetimePathMessageNl no exception, tags" <| fun _ ->
      (because "logging with LevelDatetimePathMessageNl" <| fun () ->
        sampleMessage |> StringFormatter.LevelDatetimeMessagePathNl.format)
      |> should equal (
          sprintf "E 1970-01-01T00:00:03.1234567+00:00: this is bad [a.b.c.d] {error, bad}%s"
            Environment.NewLine)
      |> thatsIt

    testCase "StringFormatter.LevelDatetimePathMessageNl no exception, tags, data" <| fun _ ->
      (because "logging with LevelDatetimePathMessageNl" <| fun () ->
        { sampleMessage with data = [ "a", box "b"; "a2", box 24 ] |> Map.ofList }
        |> StringFormatter.LevelDatetimeMessagePathNl.format)
      |> should equal (
          sprintf "E 1970-01-01T00:00:03.1234567+00:00: this is bad [a.b.c.d] {error, bad}%s  a => \"b\"%s  a2 => 24%s"
            Environment.NewLine Environment.NewLine Environment.NewLine)
      |> thatsIt

    testCase "StringFormatter.LevelDatetimePathMessageNl no exception, tags, nested data" <| fun _ ->
      (because "logging with LevelDatetimePathMessageNl" <| fun () ->
        { sampleMessage with
            data =
              [
                "a", box (["b", box 1] |> Map.ofList)
                "c", box 2
              ] |> Map.ofList
        }
        |> StringFormatter.LevelDatetimeMessagePathNl.format)
      |> should equal (
          String.Format("E 1970-01-01T00:00:03.1234567+00:00: this is bad [a.b.c.d] {{error, bad}}" + 
                        "{0}  a => {0}    b => 1{0}  c => 2{0}", Environment.NewLine))
      |> thatsIt

    testCase "StringFormatter.LevelDatetimePathMessageNl with exception, tags" <| fun _ ->
      let e = new Exception("Gremlings in the machinery")
      (because "logging with exception attached" <| fun () ->
        { sampleMessage with ``exception`` = e |> Some }
        |> StringFormatter.LevelDatetimeMessagePathNl.format)
      |> should equal (
        sprintf "E 1970-01-01T00:00:03.1234567+00:00: this is bad [a.b.c.d] {error, bad} cont...%s%O%s"
          Environment.NewLine e Environment.NewLine)
      |> thatsIt
    testCase "StringFormatter.LevelDatetimePathMessageNl with exception, tags, data" <| fun _ ->
      let e = new Exception("Gremlings in the machinery")
      (because "logging with exception attached" <| fun () ->
        { sampleMessage with
            ``exception`` = e |> Some
            data = [ "a", box "b"; "a2", box 24 ] |> Map.ofList }
        |> StringFormatter.LevelDatetimeMessagePathNl.format)
      |> should equal (
        sprintf "E 1970-01-01T00:00:03.1234567+00:00: this is bad [a.b.c.d] {error, bad}%s  a => \"b\"%s  a2 => 24 cont...%s%O%s"
          Environment.NewLine Environment.NewLine // for data
          Environment.NewLine e Environment.NewLine) // for exn
      |> thatsIt

    testCase "``JsonFormatter has no newline characters``" <| fun _ ->
      (because "logging message with newline in it" <| fun () ->
          { sampleMessage with message = "here\n  we\ngo!" } |> JsonFormatter.Default().format)
      |> should equal ("""{"message":"here\n  we\ngo!","data":{},"level":"error","tags":["error","bad"],""" +
                      """"timestamp":"1970-01-01T00:00:03.1234567Z","path":"a.b.c.d"}""")
      |> thatsIt
    ]