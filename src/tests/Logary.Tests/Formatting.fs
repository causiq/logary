module Logary.Tests.Formatting

open System
open NodaTime
open Logary
open Expecto
open Logary.KnownLiterals
open Logary

let private sampleMessage : Message =
  Message.eventError "this is bad"
  |> Message.setName (PointName.ofList ["a"; "b"; "c"; "d"])
  |> Message.setNanoEpoch 3123456700L

[<CustomEquality;CustomComparison>]
type KV = KV of string * obj
with
  override x.Equals(yobj) =
    match yobj with
    | :? KV as other ->
      let (KV (xk,xv)) = x
      let (KV (ok,ov)) = other
      xk = ok && xv = ov
    | _ -> false

  override x.GetHashCode () = hash x

  interface IComparable<KV> with
    member x.CompareTo other =
      let (KV (xk,xv)) = x
      let (KV (ok,ov)) = other
      compare xk ok

  interface IComparable with
    member x.CompareTo other =
      match other with
      | null ->
        1

      | :? KV as tother ->
        (x :> IComparable<KV>).CompareTo tother

      | _ ->
        failwithf "invalid comparison %A to %A" x other

let shouldHaveFields msg fields tip =
  msg
  |> Message.getAllFields
  |> Seq.map KV
  |> Set.ofSeq
  |> fun actual ->
     Expect.equal actual (fields |> Set.ofList) tip


let tests = [
  testCase "StringFormatter.Verbatim" <| fun _ ->
    Message.eventError "hello world"
    |> MessageWriter.verbatim.format
    |> fun actual ->
       Expect.equal actual "hello world" "formatting the message verbatim"

  testCase "StringFormatter.VerbatimNewline" <| fun _ ->
    Message.eventError "hi there"
    |> MessageWriter.verbatimNewLine.format
    |> fun actual ->
       Expect.equal actual (sprintf "hi there%s" Environment.NewLine) "formatting the message verbatim with newline"

  testCase "StringFormatter.VerbatimNewlineTemplated" <| fun _ ->
    Message.eventFormat (Info, "what's {@direction}? {up:l}!", [|"up";"up";|] )
    |> MessageWriter.verbatimNewLine.format
    |> fun actual ->
       Expect.equal actual (sprintf "what's \"up\"? up!%s" Environment.NewLine) "formatting the message verbatim with newline, templated"

  testCase "StringFormatter.VerbatimNewlineTemplated.WithFields" <| fun _ ->
    skiptest ("depend on will we continue support Field, if we don't, no need test." +
             "if we do, we can custom destructure for Field/Value type in MessageWriterModuel," +
             "then can preserve the origin representation")

    // Message.eventFormat (Info, "what's {@direction}", [|Field (String "up", None)|] )
    // |> MessageWriter.verbatimNewLine.format
    // |> fun actual ->
    //    Expect.equal actual (sprintf "what's up%s" Environment.NewLine) "formatting the message verbatim with newline, templated"

  testCase "StringFormatter.LevelDatetimePathMessageNl no exception" <| fun _ ->
    sampleMessage
    |> MessageWriter.levelDatetimeMessagePathNewLine.format
    |> fun actual ->
       Expect.equal actual (
         sprintf "E 1970-01-01T00:00:03.1234567+00:00: this is bad [a.b.c.d]%s"
           Environment.NewLine) "formatting the message LevelDatetimePathMessageNl"

  testCase "StringFormatter.LevelDatetimePathMessage no exception, data" <| fun _ ->
    sampleMessage
    |> Message.setContextValues [("a",box "b");("a2", box 24)]
    |> MessageWriter.levelDatetimeMessagePath.format
    |> fun actual ->
       let expect = """
E 1970-01-01T00:00:03.1234567+00:00: this is bad [a.b.c.d]
  context:
    others:
      a2 => 24
      a => "b"
"""
       Expect.equal actual (expect.Trim([|'\r';'\n'|])) "formatting the message LevelDatetimePathMessage"


  testCase "StringFormatter.LevelDatetimePathMessageNl no exception, data" <| fun _ ->
    sampleMessage
    |> Message.setContextValues [("a",box "b");("a2", box 24)]
    |> MessageWriter.levelDatetimeMessagePathNewLine.format
    |> fun actual ->
       let expect = """
E 1970-01-01T00:00:03.1234567+00:00: this is bad [a.b.c.d]
  context:
    others:
      a2 => 24
      a => "b"
"""
       Expect.equal actual (expect.TrimStart([|'\r';'\n'|])) "formatting the message LevelDatetimePathMessageNl"

  testCase "StringFormatter.LevelDatetimePathMessageNl no exception, data, list with map in it" <| fun _ ->
    sampleMessage
    |> Message.setContextValues [("a", box "b"); ("a2", box 24); ("things", box [| box 1; box 2; box (Map ["1",box "hello";"2",box 42]) |])]
    |> MessageWriter.levelDatetimeMessagePathNewLine.format
    |> fun actual ->
       let expect = """
E 1970-01-01T00:00:03.1234567+00:00: this is bad [a.b.c.d]
  context:
    others:
      a2 => 24
      a => "b"
      things => 
        - 1
        - 2
        - 
          "1" => "hello"
          "2" => 42
"""
       Expect.equal actual (expect.TrimStart([|'\r';'\n'|])) "formatting the message LevelDatetimePathMessageNl"

  testCase "StringFormatter.LevelDatetimePathMessageNl no exception, nested data" <| fun _ ->
    let foo = [1;2;3;4]
    sampleMessage
    |> Message.setContextValues [("a", box (Map ["b", 1])); ("c", box 2);("d",box (Map [[1,2],["3";"4"];[1,2;3,4],["7";"8"]]));]
    |> MessageWriter.levelDatetimeMessagePathNewLine.format
    |> fun actual ->
       let expect = """
E 1970-01-01T00:00:03.1234567+00:00: this is bad [a.b.c.d]
  context:
    others:
      d => 
        - key => 
            - 
              - 1
              - 2
          value => 
            - "3"
            - "4"
        - key => 
            - 
              - 1
              - 2
            - 
              - 3
              - 4
          value => 
            - "7"
            - "8"
      a => 
        "b" => 1
      c => 2
"""
       Expect.equal actual (expect.TrimStart([|'\r';'\n'|])) "formatting the message LevelDatetimePathMessageNl"

  testCase "StringFormatter.LevelDatetimePathMessageNl with exception" <| fun _ ->
    let inner = new Exception("inner exception")
    let e = new Exception("Gremlings in the machinery", inner)
    sampleMessage
    |> Message.addExn e
    |> MessageWriter.levelDatetimeMessagePathNewLine.format
    |> fun actual ->
       let expect = """
E 1970-01-01T00:00:03.1234567+00:00: this is bad [a.b.c.d]
  context:
    others:
      _logary.errors => 
        - 
          Exception {
            Message => "Gremlings in the machinery"
            Data => 
            InnerException => 
              Exception {
                Message => "inner exception"
                Data => 
                InnerException => null
                TargetSite => null
                StackTrace => null
                HelpLink => null
                Source => null
                HResult => -2146233088}
            TargetSite => null
            StackTrace => null
            HelpLink => null
            Source => null
            HResult => -2146233088}
"""
       Expect.equal actual (expect.TrimStart([|'\r';'\n'|]))
         "formatting the message LevelDatetimePathMessageNl with exception attached"

  testCase "StringFormatter.LevelDatetimePathMessageNl with exception, data" <| fun _ ->
    let e = new Exception("Gremlings in the machinery")
    sampleMessage
    |> Message.addExn e
    |> Message.setContextValues [("a", box "b"); ("a2", box 24);]
    |> MessageWriter.levelDatetimeMessagePathNewLine.format
    |> fun actual ->
       let expect = """
E 1970-01-01T00:00:03.1234567+00:00: this is bad [a.b.c.d]
  context:
    others:
      _logary.errors => 
        - 
          Exception {
            Message => "Gremlings in the machinery"
            Data => 
            InnerException => null
            TargetSite => null
            StackTrace => null
            HelpLink => null
            Source => null
            HResult => -2146233088}
      a2 => 24
      a => "b"
"""
       Expect.equal actual (expect.TrimStart([|'\r';'\n'|]))
         "formatting the message LevelDatetimePathMessageNl with exception attached"

  testCase "``JsonFormatter has no newline characters``" <| fun _ ->
    skiptest "use fspickler, maybe should support in another project inherit MessageWriter with fspickler as its dependency"
    // (because "logging message with newline in it" <| fun () ->
    //     { sampleMessage with value = Event "here\n  we\ngo!" } |> JsonFormatter.Default.format)
    // |> should equal ("""{"context":{},"fields":{},"level":"error","name":["a","b","c","d"],""" +
    //                  """"timestamp":3123456700,"value":{"event":"here\n  we\ngo!"}}""")
    // |> thatsIt

  testCase "Formatting.templateFormat, simple case" <| fun _ ->
    let format = "This {0} contains {1} words."
    let args : obj[] = [|"sentence"; 4|]
    let msg = Message.templateFormat(format, args)
    shouldHaveFields msg [KV("0","sentence"); KV("1",4)] "converting a String.Format into a message template"

  testCase "Formatting.templateFormat, named and positional fields" <| fun _ ->
    let format = "This {gramaticalStructure} contains {wordCount} {0}."
    let args : obj[] = [|"sentence"; 4; "words"|]

    let msg = Message.templateFormat(format, args)
    shouldHaveFields msg [KV("gramaticalStructure","sentence"); KV("wordCount",4);KV("0","words")]
      "fields are matched left-to-right when any fields are named"

  testCase "Formatting.templateFormat, positional fields" <| fun _ ->
    let format = "Positionally - two {2} . {2} . zero {0} . {0}"
    let args : obj[] = [|0;1;2;3|]

    let msg = Message.templateFormat(format, args)
    shouldHaveFields msg [KV("0", 0); KV("2", 2);]
      "fields are matched positionally when all are numbered"

  testCase "Formatting.templateFormat, named fields" <| fun _ ->
    let format = "This {gramaticalStructure} contains {wordCount} words."
    let args : obj[] = [|"sentence"; 4|]

    let msg = Message.templateFormat(format, args)
    shouldHaveFields msg [KV("gramaticalStructure","sentence"); KV("wordCount", 4);]
      "fields are matched left-to-right in message template"

  testCase "Formatting.templateFormat, named fields, missing last" <| fun _ ->
    let format = "This {gramaticalStructure} contains {wordCount} words."
    let args : obj[] = [|"sentence"|]

    let msg = Message.templateFormat(format, args)
    shouldHaveFields msg [KV ("gramaticalStructure", "sentence")] "fields are matched left-to-right in message template"

  testCase "Formatting.templateFormat, named fields, all missing" <| fun _ ->
    let format = "This {gramaticalStructure} contains {wordCount} words."
    let args : obj[] = [||]

    let msg = Message.templateFormat(format, args)
    shouldHaveFields msg [] "fields are matched left-to-right in message template"

  testCase "templateEvent<_> reconises the '$' symbol and will call 'ToString()' on the captured value" <| fun _ ->
    let stringifyLogEvent = Message.templateEvent<Version>(Info, "Found version {$Version}")
    let version = System.Version(1,2,3,4)
    let msg = stringifyLogEvent version

    shouldHaveFields msg [KV ("Version", version)] "should set field"

    msg
    |> MessageWriter.verbatim.format
    |> fun actual ->
       Expect.stringContains actual "1.2.3.4" "should call tostring() on version"

  testCase "templateEvent<_> reconises the '@' symbol and will extract the properties of the captured value" <| fun _ ->
    let structureLogEvent = Message.templateEvent<Version>(Info, "Found version {@Version}")
    let version = System.Version(1,2,3,4)
    let msg = structureLogEvent version

    shouldHaveFields msg [KV ("Version", version)] "should set field"

    msg
    |> MessageWriter.verbatim.format
    |> fun actual ->
       Expect.stringContains actual "Build" "Should extract properties"
       Expect.stringContains actual "Major" "Should extract properties"
       Expect.stringContains actual "MajorRevision" "Should extract properties"
       Expect.stringContains actual "Minor" "Should extract properties"
       Expect.stringContains actual "MinorRevision" "Should extract properties"
       Expect.stringContains actual "Revision" "Should extract properties"
       Expect.stringContains actual "Version" "Should have typetag"

  testCase "templateEvent<_> works with one to four type params" <| fun _ ->
    let logEventGuid      = Message.templateEvent<Guid>            (Info, "This special {Guid} is logged")
    let logEventStringInt = Message.templateEvent<string, int>     (Warn, "This {gramaticalStructure} contains {wordCount} words.")
    let logEventIntIntInt = Message.templateEvent<int, int, int>   (Error, "There a 3 numbers: {one} {two} {three}")
    let logEventExns      = Message.templateEvent<exn,exn,exn,exn> (Fatal, "There a 4 exns: {one} {two} {three} {four}")

    shouldHaveFields (logEventGuid Guid.Empty) [KV ("Guid", Guid.Empty)] "should set field"
    shouldHaveFields (logEventStringInt "sentence" 30) [KV("gramaticalStructure", "sentence"); KV("wordCount", 30)] "should set field"
    shouldHaveFields  (logEventIntIntInt 1 2 3) [KV("one", 1);KV("two", 2); KV("three",3)] "should set field"
    let (e1,e2,e3,e4) = (exn "1"), (exn "2"), (exn "3"), (exn "4")
    shouldHaveFields (logEventExns e1 e2 e3 e4) [KV("one", e1); KV("two", e2); KV("three", e3); KV("four", e4)] "should set field"

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
