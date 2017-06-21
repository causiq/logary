module Logary.Tests.Formatting

open Expecto
open System
open NodaTime
open Logary
open Logary.Formatting
open Logary.Tests.TestDSL

let private sampleMessage : Message =
  { name      = PointName.ofList ["a"; "b"; "c"; "d"]
    value     = Event "this is bad"
    fields    = Map.empty
    context   = Map.empty
    timestamp = Instant.FromUnixTimeSeconds(3L).PlusTicks(1234567L).ToUnixTimeTicks() * 100L
    level     = LogLevel.Error }

let extractFormatFields msg =
  let template =
    match msg.value with
    | Event template ->
      template

    | x -> Tests.failtestf "unexpected %A" x

  let fields =
    msg.fields
    |> Seq.map (fun (KeyValue (key, value)) -> PointName.format key, value)

  template, Set.ofSeq fields

[<Tests>]
let tests =
  testList "formatting" [
    testCase "StringFormatter.Verbatim" <| fun _ ->
      (because "formatting the message verbatim" <| fun _ ->
        Message.eventError "hello world"
        |> StringFormatter.verbatim.format)
      |> should equal "hello world"
      |> thatsIt

    testCase "StringFormatter.VerbatimNewline" <| fun _ ->
      (because "logging verbatim with newline" <| fun () ->
        Message.eventInfo "hi there"
        |> StringFormatter.verbatimNewLine.format)
      |> should equal (sprintf "hi there%s" Environment.NewLine)
      |> thatsIt

    testCase "StringFormatter.VerbatimNewlineTemplated" <| fun _ ->
      (because "logging verbatim with newline, templated" <| fun () ->
        {(Message.eventInfo "what's {@direction}") with fields = [(PointName.ofSingle "direction", Field (String "up", None))] |> Map.ofList}
        |> StringFormatter.verbatimNewLine.format)
      |> should equal (sprintf "what's up%s" Environment.NewLine)
      |> thatsIt

    testCase "StringFormatter.LevelDatetimePathMessageNl no exception" <| fun _ ->
      (because "logging with LevelDatetimePathMessageNl" <| fun () ->
        sampleMessage |> StringFormatter.levelDatetimeMessagePathNl.format)
      |> should equal (
          sprintf "E 1970-01-01T00:00:03.1234567+00:00: this is bad [a.b.c.d]%s"
            Environment.NewLine)
      |> thatsIt

    testCase "StringFormatter.LevelDatetimePathMessageNl no exception, data" <| fun _ ->
      (because "logging with LevelDatetimePathMessageNl" <| fun () ->
        { sampleMessage with
            fields = Map [PointName.ofSingle "a", Field (String "b", None); PointName.ofSingle "a2", Field (Int64 24L, None) ]
            context = Map ["a", String "b"]}
        |> StringFormatter.levelDatetimeMessagePathNl.format)
      |> should equal (
          sprintf "E 1970-01-01T00:00:03.1234567+00:00: this is bad [a.b.c.d]%s  a => \"b\"%s  a2 => 24%s  Context:%s    a => \"b\"%s"
            Environment.NewLine Environment.NewLine Environment.NewLine Environment.NewLine Environment.NewLine)
      |> thatsIt

    testCase "StringFormatter.LevelDatetimePathMessageNl no exception, data, list with map in it" <| fun _ ->
      (because "logging with LevelDatetimePathMessageNl" <| fun () ->
        { sampleMessage with
            fields = [ PointName.ofSingle "a",  (Field (String "b", None))
                       PointName.ofSingle "a2", (Field (Int64 24L, None))
                       PointName.ofSingle "things",
                         (Field (Array
                           [ Int64 1L
                             Int64 2L
                             Object <| Map ["1", String "hello"] ], None))
                     ] |> Map.ofList
        }
        |> StringFormatter.levelDatetimeMessagePathNl.format)
      |> should equal (
          String.Format("E 1970-01-01T00:00:03.1234567+00:00: this is bad [a.b.c.d]" +
                        "{0}  a => \"b\"{0}  a2 => 24{0}  things => {0}    - 1{0}    - 2{0}    - {0}      1 => \"hello\"{0}",
                        Environment.NewLine))
      |> thatsIt

    testCase "StringFormatter.LevelDatetimePathMessageNl no exception, nested data" <| fun _ ->
      (because "logging with LevelDatetimePathMessageNl" <| fun () ->
        { sampleMessage with
            fields =
              [ PointName.ofSingle "a", (Field (["b", Int64 1L] |> Map.ofList |> Object, None))
                PointName.ofSingle "c", (Field (Int64 2L, None))
              ] |> Map.ofList
        }
        |> StringFormatter.levelDatetimeMessagePathNl.format)
      |> should equal (
          String.Format("E 1970-01-01T00:00:03.1234567+00:00: this is bad [a.b.c.d]" +
                        "{0}  a => {0}    b => 1{0}  c => 2{0}", Environment.NewLine))
      |> thatsIt

    testCase "StringFormatter.LevelDatetimePathMessageNl with exception" <| fun _ ->
      let e = new Exception("Gremlings in the machinery")
      (because "logging with exception attached" <| fun () ->
        sampleMessage
        |> Message.addExn e
        |> StringFormatter.levelDatetimeMessagePathNl.format)
      |> should equal (
        String.Format("E 1970-01-01T00:00:03.1234567+00:00: this is bad [a.b.c.d]" +
                      "{0}  errors => {0}    - {0}      hResult => -2146233088{0}      message => \"{1}\"{0}      type => \"{2}\"{0}",
                      Environment.NewLine, e.Message, (e.GetType ()).FullName))
      |> thatsIt

    testCase "StringFormatter.LevelDatetimePathMessageNl with exception, data" <| fun _ ->
      let e = new Exception("Gremlings in the machinery")
      (because "logging with exception attached" <| fun () ->
        { sampleMessage with fields = [PointName.ofSingle "a", Field (String "b", None); PointName.ofSingle "a2", Field (Int64 24L, None) ] |> Map.ofList }
        |> Message.addExn e
        |> StringFormatter.levelDatetimeMessagePathNl.format)
      |> should equal (
        String.Format("E 1970-01-01T00:00:03.1234567+00:00: this is bad [a.b.c.d]"+
                      "{0}  a => \"b\"{0}  a2 => 24{0}" +
                      "  errors => {0}    - {0}      hResult => -2146233088{0}      message => \"{1}\"{0}      type => \"{2}\"{0}",
                      Environment.NewLine, e.Message, (e.GetType ()).FullName))
      |> thatsIt

    testCase "``JsonFormatter has no newline characters``" <| fun _ ->
      (because "logging message with newline in it" <| fun () ->
          { sampleMessage with value = Event "here\n  we\ngo!" } |> JsonFormatter.Default.format)
      |> should equal ("""{"context":{},"fields":{},"level":"error","name":["a","b","c","d"],""" +
                       """"timestamp":3123456700,"value":{"event":"here\n  we\ngo!"}}""")
      |> thatsIt

    testCase "Formatting.templateFormat, simple case" <| fun _ ->
      let format = "This {0} contains {1} words."
      let args : obj[] = [|"sentence"; 4|]
      (because "converting a String.Format into a message template" <| fun () ->
        extractFormatFields (Message.templateFormat(format, args)))
      |> should equal ("This {0} contains {1} words.",
                       Set [ "0", Field (String "sentence", None)
                             "1", Field (Int64 4L, None) ])
      |> thatsIt
      
    testCase "Formatting.templateFormat, named and positional fields" <| fun _ ->
      let format = "This {gramaticalStructure} contains {wordCount} {0}."
      let args : obj[] = [|"sentence"; 4; "words"|]
      (because "fields are matched left-to-right when any fields are named" <| fun () ->
        extractFormatFields (Message.templateFormat(format, args)))
      |> should equal ("This {gramaticalStructure} contains {wordCount} {0}.",
                       Set [ "gramaticalStructure", Field (String "sentence", None)
                             "wordCount", Field (Int64 4L, None)
                             "0", Field (String "words", None) ])
      |> thatsIt

    testCase "Formatting.templateFormat, positional fields" <| fun _ ->
      let format = "Positionally - two {2} . {2} . zero {0} . {0}"
      let args : obj[] = [|0;1;2;3|] 
      (because "fields are matched positionally when all are numbered" <| fun () ->
        extractFormatFields (Message.templateFormat(format, args)))
      |> should equal ("Positionally - two {2} . {2} . zero {0} . {0}",
                       Set [ ("0", Field (Int64 0L, None))
                             ("2", Field (Int64 2L, None)) ])
      |> thatsIt

    testCase "Formatting.templateFormat, named fields" <| fun _ ->
      let format = "This {gramaticalStructure} contains {wordCount} words."
      let args : obj[] = [|"sentence"; 4|]
      (because "fields are matched left-to-right in message template" <| fun () ->
        extractFormatFields (Message.templateFormat(format, args)))
      |> should equal ("This {gramaticalStructure} contains {wordCount} words.",
                       Set [ "gramaticalStructure", Field (String "sentence", None)
                             "wordCount", Field (Int64 4L, None) ])
      |> thatsIt

    testCase "Formatting.templateFormat, named fields, missing last" <| fun _ ->
      let format = "This {gramaticalStructure} contains {wordCount} words."
      let args : obj[] = [|"sentence"|]
      (because "fields are matched left-to-right in message template" <| fun () ->
        extractFormatFields (Message.templateFormat(format, args)))
      |> should equal ("This {gramaticalStructure} contains {wordCount} words.",
                       Set [ "gramaticalStructure", Field (String "sentence", None) ])
      |> thatsIt

    testCase "Formatting.templateFormat, named fields, all missing" <| fun _ ->
      let format = "This {gramaticalStructure} contains {wordCount} words."
      let args : obj[] = [||]
      (because "fields are matched left-to-right in message template" <| fun () ->
        extractFormatFields (Message.templateFormat(format, args)))
      |> should equal ("This {gramaticalStructure} contains {wordCount} words.",
                       Set [ ])
      |> thatsIt

    testCase "templateEvent<_> reconises the '$' symbol and will call 'ToString()' on the captured value" <| fun _ ->
      let stringifyLogEvent = Message.templateEvent<Version>(Info, "Found version {$Version}")
      (because "" <| fun () -> extractFormatFields (stringifyLogEvent (Version(1,2,3,4))))
      |> should equal ( "Found version {$Version}", Set [ "Version", Field (String "1.2.3.4", None) ] )
      |> thatsIt

    testCase "templateEvent<_> reconises the '@' symbol and will extract the properties of the captured value" <| fun _ ->
      let structureLogEvent = Message.templateEvent<Version>(Info, "Found version {@Version}")
      let expectedCapturedFields =
        Map [ "Build",          Int64 3L
              "Major",          Int64 1L
              "MajorRevision",  Int64 0L
              "Minor",          Int64 2L
              "MinorRevision",  Int64 4L
              "Revision",       Int64 4L
              "_typeTag",       String "Version"]
      (because "" <| fun () -> extractFormatFields (structureLogEvent (Version(1,2,3,4))))
      |> should equal (
          "Found version {@Version}",
            Set [ "Version", Field (Object expectedCapturedFields, None) ])
      |> thatsIt

    testCase "templateEvent<_> works with one to four type params" <| fun _ ->
      let logEventGuid      = Message.templateEvent<Guid>            (Info, "This special {Guid} is logged")
      let logEventStringInt = Message.templateEvent<string, int>     (Warn, "This {gramaticalStructure} contains {wordCount} words.")
      let logEventIntIntInt = Message.templateEvent<int, int, int>   (Error, "There a 3 numbers: {one} {two} {three}")
      let logEventExns      = Message.templateEvent<exn,exn,exn,exn> (Fatal, "There a 4 exns: {one} {two} {three} {four}")

      // Because enx.ToString() will be called
      let exnField msg = Field (String ("System.Exception: " + msg), None)

      (because "templateEvent<_> functions generate a message with field values captured and named correctly" <| fun () ->
        extractFormatFields (logEventGuid Guid.Empty)
        , extractFormatFields (logEventStringInt "sentence" 30)
        , extractFormatFields (logEventIntIntInt 1 2 3)
        , extractFormatFields (logEventExns (exn "1") (exn "2") (exn "3") (exn "4")))
      |> should equal (
          ("This special {Guid} is logged",
              Set [ "Guid", Field (String ("00000000-0000-0000-0000-000000000000"), None) ]),
          ("This {gramaticalStructure} contains {wordCount} words.",
              Set [ "gramaticalStructure", Field (String "sentence", None)
                    "wordCount", Field (Int64 30L, None) ]),
          ("There a 3 numbers: {one} {two} {three}",
              Set [ "one", Field (Int64 1L, None)
                    "two", Field (Int64 2L, None)
                    "three", Field (Int64 3L, None) ]),
          ("There a 4 exns: {one} {two} {three} {four}",
              Set [ "one",    exnField "1"
                    "two",    exnField "2"
                    "three",  exnField "3"
                    "four",   exnField "4" ])
      ) |> thatsIt

    testCase "templateEvent<_> throws when there are positionally matched fields" <| fun _ ->
      Expect.throws (fun () -> Message.templateEvent<int> (Info, "No named fields {0}") |> ignore)
                    "No named fields passed 1 gen par"
      Expect.throws (fun () -> Message.templateEvent<int, int> (Info, "No named fields {0} {1}") |> ignore)
                    "No named fields passed 2 gen pars"
      Expect.throws (fun () -> Message.templateEvent<int, int, int> (Info, "No named fields {0} {1} {2}") |> ignore)
                    "No named fields passed 3 gen pars"
      Expect.throws (fun () -> Message.templateEvent<int, int, int, int> (Info, "No named fields {0} {1} {2} {3}") |> ignore)
                    "No named fields passed 4 gen pars"

    testCase "templateEvent<_> requires exactly the same number of type args and properties in the template" <| fun _ ->
      Expect.throws (fun () -> Message.templateEvent<int> (Info, "Too many {Field1} {Field2}") |> ignore)
                    "Missing one type arg"
      Expect.throws (fun () -> Message.templateEvent<int> (Info, "Too many {Field1} {Field2} {Field3}") |> ignore)
                    "Missing two type args"
      Expect.throws (fun () -> Message.templateEvent<int> (Info, "Too few") |> ignore)
                    "One type arg too many"

      Expect.throws (fun () -> Message.templateEvent<int, int> (Info, "Too many {Field1} {Field2} {Field3}") |> ignore)
                    "Missing one type arg"
      Expect.throws (fun () -> Message.templateEvent<int, int> (Info, "Too many {Field1} {Field2} {Field3} {Field4}") |> ignore)
                    "Missing two type args"
      Expect.throws (fun () -> Message.templateEvent<int, int> (Info, "Too few") |> ignore)
                    "Two type args too many"
      Expect.throws (fun () -> Message.templateEvent<int, int> (Info, "Too few {Field1}") |> ignore)
                    "One type args too many"

      Expect.throws (fun () -> Message.templateEvent<int, int, int> (Info, "Too many {Field1} {Field2} {Field3} {Field4}") |> ignore)
                    "One type args too few"
      Expect.throws (fun () -> Message.templateEvent<int, int, int> (Info, "Too many {Field1} {Field2} {Field3} {Field4} {Field5}") |> ignore)
                    "Two type args too few"
      Expect.throws (fun () -> Message.templateEvent<int, int, int> (Info, "Too few") |> ignore)
                    "Three type args too many"
      Expect.throws (fun () -> Message.templateEvent<int, int, int> (Info, "Too few {Field1}") |> ignore)
                    "Two type args too many"
      Expect.throws (fun () -> Message.templateEvent<int, int, int> (Info, "Too few {Field1} {Field2}") |> ignore)
                    "One type arg too many"

      Expect.throws (fun () -> Message.templateEvent<int, int, int, int> (Info, "Too many {Field1} {Field2} {Field3} {Field4} {Field5}") |> ignore)
                    "Missing one type arg"
      Expect.throws (fun () -> Message.templateEvent<int, int, int, int> (Info, "Too many {Field1} {Field2} {Field3} {Field4} {Field5} {Field6}") |> ignore)
                    "Missing two type args"
      Expect.throws (fun () -> Message.templateEvent<int, int, int, int> (Info, "Too few") |> ignore)
                    "Four type args too many"
      Expect.throws (fun () -> Message.templateEvent<int, int, int, int> (Info, "Too few {Field1}") |> ignore)
                    "Three type args too many"
      Expect.throws (fun () -> Message.templateEvent<int, int, int, int> (Info, "Too few {Field1} {Field2}") |> ignore)
                    "Two type args too many"
      Expect.throws (fun () -> Message.templateEvent<int, int, int, int> (Info, "Too few {Field1} {Field2} {Field3}") |> ignore)
                    "One type arg too many"

    ]
