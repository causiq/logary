module Logary.Tests.Formatting

open System
open NodaTime
open Logary
open Expecto
open Logary.KnownLiterals
open Logary
open Logary.MessageWriter
open Logary.MessageTemplates.Destructure
open Logary.MessageTemplates
open System.Diagnostics

let private sampleMessage : Message =
  Message.eventFormat (Info, "this is bad, with {1} and {0} reverse.", "the first value", "the second value")
  |> Message.setName (PointName.ofList ["a"; "b"; "c"; "d"])
  |> Message.setNanoEpoch 3123456700L

type User = 
  {
    id      : int 
    name    : string
    created : DateTime
  }

type Obj() =
  member x.PropA =
    45
  member x.PropB =
    raise (Exception ("Oh noes, no referential transparency here"))
with
  interface IFormattable with
    member x.ToString (format, provider) = "PropA is 45 and PropB raise exn"

let date20171111 =  DateTime.Parse("2017-11-11")
let foo () = { id = 999; name = "whatever"; created = date20171111}


let complexMessage : Message =
  let ex = exn "exception with data in it"
  ex.Data.Add ("data 1 in exn", 1)
  ex.Data.Add ("data foo in exn", foo ())
  ex.Data.Add (foo(), foo())

  let tp () = (1, "two", foo())
  let (scalarArr : obj[]) = [| 1;  2; 3; "4"; "5"; 6.0; date20171111 |]
  let (notScalarList : obj list) = [foo (); tp ()] 
  let scalarKeyValueMap = [ 1,"one" ; 2, "two"] |> HashMap.ofSeq
  let scalarKeyMap = Map [ "some user", box (foo ()) ; "some obj", box (Obj())]
  let notScalarMap = Map [([2,"2"],["3";"4"]); ([1,"a";2,"b"],["hello";"world"])]

  Message.eventFormat (Info, 
    "default foo is {foo} here is a default {objDefault} and stringify {$objStr} and destructure {@objDestr}", 
    foo (), Obj(),  Obj(),  Obj())
  |> Message.setName  (PointName.ofList ["a"; "b"; "c"; "d"])
  |> Message.setNanoEpoch 3123456700L
  |> Message.setContext "UserInfo" (foo ())
  |> Message.setContext "Some Tuple With 1 two foo" (tp ())
  |> Message.setContext "scalar array" scalarArr
  |> Message.setContext "no scalar list" notScalarList
  |> Message.setContext "simple scalar key/value map" scalarKeyValueMap
  |> Message.setContext "just scalar key map" scalarKeyMap
  |> Message.setContext "no scalar key/value map" notScalarMap
  |> Message.addGauge "svc1 request per second" (Gauge(1750., Units.Scalar))
  |> Message.addGauge "Processor.% Idle.Core 1" (Gauge(0.75, Units.Percent))
  |> Message.addGauge "methodA" (Gauge(25000000000., Units.Scaled (Seconds, float Constants.NanosPerSecond)))
  |> Message.addExn ex
  |> Message.addExn (exn "another exception")

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

// set have order
let shouldHaveFields msg fields tip =
  msg
  |> Message.getAllFields
  |> Seq.map KV
  |> Set.ofSeq
  |> fun actual ->
     Expect.equal actual (fields |> Set.ofList) tip

// just for test convenient, since file end of line is LF.
let levelDatetimeMessagePathNewLine =
  expanded "\n" "\n"

type ProjectionTestOnly =
  {
    ex: exn
    user: User
  }

type ProjectionTestExcept =
  {
    user: User
  }

let tests = [
  testCase "cycle reference" <| fun _ ->
    let actual = Logary.MessageTemplates.Formatting.Format("stringify: {$0} default: {0} structure: {@0}",System.Threading.Thread.CurrentPrincipal)
    Expect.equal actual """stringify: "System.Security.Principal.GenericPrincipal" default: "System.Security.Principal.GenericPrincipal" structure: GenericPrincipal { Identity: GenericIdentity { RoleClaimType: "http://schemas.microsoft.com/ws/2008/06/identity/claims/role", NameClaimType: "http://schemas.xmlsoap.org/ws/2005/05/identity/claims/name", Name: "", Label: null, IsAuthenticated: False, Claims: [Claim { ValueType: "http://www.w3.org/2001/XMLSchema#string", Value: "", Type: "http://schemas.xmlsoap.org/ws/2005/05/identity/claims/name", Subject: $12 , Properties: [], OriginalIssuer: "LOCAL AUTHORITY", Issuer: "LOCAL AUTHORITY" }], BootstrapContext: null, AuthenticationType: "", Actor: null }, Identities: [$5 ], Claims: [Claim { ValueType: "http://www.w3.org/2001/XMLSchema#string", Value: "", Type: "http://schemas.xmlsoap.org/ws/2005/05/identity/claims/name", Subject: GenericIdentity { RoleClaimType: "http://schemas.microsoft.com/ws/2008/06/identity/claims/role", NameClaimType: "http://schemas.xmlsoap.org/ws/2005/05/identity/claims/name", Name: "", Label: null, IsAuthenticated: False, Claims: [$3 , Claim { ValueType: "http://www.w3.org/2001/XMLSchema#string", Value: "", Type: "http://schemas.microsoft.com/ws/2008/06/identity/claims/role", Subject: $5 , Properties: [], OriginalIssuer: "LOCAL AUTHORITY", Issuer: "LOCAL AUTHORITY" }], BootstrapContext: null, AuthenticationType: "", Actor: null }, Properties: [], OriginalIssuer: "LOCAL AUTHORITY", Issuer: "LOCAL AUTHORITY" }, Claim { ValueType: "http://www.w3.org/2001/XMLSchema#string", Value: "", Type: "http://schemas.microsoft.com/ws/2008/06/identity/claims/role", Subject: $5 , Properties: [], OriginalIssuer: "LOCAL AUTHORITY", Issuer: "LOCAL AUTHORITY" }] }""" "cycle reference should work"

  testCase "projection only" <| fun _ ->
    let only = <@@ Destructure.only<ProjectionTestOnly>(fun foo -> [|foo.user.created.Day;foo.ex.Message;foo.ex.StackTrace;foo.ex.Data.Count;foo.ex.InnerException.Message|]) @@>
    Logary.Configuration.Config.configProjection only
    
    let inner = exn "inner exception"
    let e = new Exception("top", inner)
    e.Data.Add(1,2)
    e.Data.Add(3,4)

    sampleMessage
    |> Message.setContext "only" {ex = e; user= (foo ())}
    |> levelDatetimeMessagePathNewLine.format
    |> fun actual ->
       let expect = """
I 1970-01-01T00:00:03.1234567+00:00: this is bad, with "the second value" and "the first value" reverse. [a.b.c.d]
  fields:
    0 => "the first value"
    1 => "the second value"
  others:
    only => 
      ProjectionTestOnly {
        user => 
          User {
            created => 
              DateTime {
                Day => 11}}
        ex => 
          Exception {
            StackTrace => null
            Message => "top"
            InnerException => 
              Exception {
                Message => "inner exception"}
            Data => 
              ListDictionaryInternal {
                Count => 2}}}
"""
       Expect.equal actual (expect.TrimStart([|'\n'|]))
         "formatting the message LevelDatetimePathMessageNl with projection"


  testCase "projection except" <| fun _ ->
    let except = <@@  Destructure.except<ProjectionTestExcept>(fun t -> [|t.user.created.Date|]) @@>
    let invalid = <@@ 1 + 1 @@>
    Logary.Configuration.Config.configProjection except
    Logary.Configuration.Config.configProjection invalid
    
    sampleMessage
    |> Message.setContext "except" {user= (foo ())}
    |> levelDatetimeMessagePathNewLine.format
    |> fun actual ->
       let expect = """
I 1970-01-01T00:00:03.1234567+00:00: this is bad, with "the second value" and "the first value" reverse. [a.b.c.d]
  fields:
    0 => "the first value"
    1 => "the second value"
  others:
    except => 
      ProjectionTestExcept {
        user => 
          User {
            name => "whatever"
            id => 999
            created => 
              DateTime {
                Year => 2017
                TimeOfDay => 00:00:00
                Ticks => 636459552000000000
                Second => 0
                Month => 11
                Minute => 0
                Millisecond => 0
                Kind => "Unspecified"
                Hour => 0
                DayOfYear => 315
                DayOfWeek => "Saturday"
                Day => 11}}}
"""
       Expect.equal actual (expect.TrimStart([|'\n'|]))
         "formatting the message LevelDatetimePathMessageNl with projection"

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
    Message.eventFormat (Info, "what's {@direction}? {up:l}!", "up","up")
    |> MessageWriter.verbatimNewLine.format
    |> fun actual ->
       Expect.equal actual (sprintf "what's \"up\"? up!%s" Environment.NewLine) "formatting the message verbatim with newline, templated"

  testCase "StringFormatter.VerbatimNewlineTemplated.WithFields" <| fun _ ->
    skiptest ("depend on will we continue support Field, if we don't, no need test." +
             "if we do, we can custom destructure for Field/Value type in LiterateFormatting.MessageParts," +
             "like what gauges do, then can preserve the origin representation")

    // Message.eventFormat (Info, "what's {@direction}", [|Field (String "up", None)|] )
    // |> MessageWriter.verbatimNewLine.format
    // |> fun actual ->
    //    Expect.equal actual (sprintf "what's up%s" Environment.NewLine) "formatting the message verbatim with newline, templated"

  testCase "StringFormatter.LevelDatetimePathMessageNl no exception" <| fun _ ->
    sampleMessage
    |> levelDatetimeMessagePathNewLine.format
    |> fun actual ->
       let expect = """
I 1970-01-01T00:00:03.1234567+00:00: this is bad, with "the second value" and "the first value" reverse. [a.b.c.d]
  fields:
    0 => "the first value"
    1 => "the second value"
"""
       Expect.equal actual (expect.TrimStart([|'\n'|])) "formatting the message LevelDatetimePathMessageNl"


  testCase "StringFormatter.LevelDatetimePathMessageNl with exception" <| fun _ ->
    let inner = new Exception("inner exception")
    let e = new Exception("Gremlings in the machinery", inner)
    sampleMessage
    |> Message.addExn e
    |> levelDatetimeMessagePathNewLine.format
    |> fun actual ->
       let expect = """
I 1970-01-01T00:00:03.1234567+00:00: this is bad, with "the second value" and "the first value" reverse. [a.b.c.d]
  fields:
    0 => "the first value"
    1 => "the second value"
  others:
    _logary.errors => 
      - 
        Exception {
          TargetSite => null
          StackTrace => null
          Source => null
          Message => "Gremlings in the machinery"
          InnerException => 
            Exception {
              TargetSite => null
              StackTrace => null
              Source => null
              Message => "inner exception"
              InnerException => null
              HelpLink => null
              HResult => -2146233088
              Data => }
          HelpLink => null
          HResult => -2146233088
          Data => }
"""
       Expect.equal actual (expect.TrimStart([|'\n'|]))
         "formatting the message LevelDatetimePathMessageNl with exception attached"

  testCase "StringFormatter.LevelDatetimePathMessageNl complex data" <| fun _ ->
    complexMessage
    |> levelDatetimeMessagePathNewLine.format
    |> fun actual ->
       let expect = """
I 1970-01-01T00:00:03.1234567+00:00: default foo is "{id = 999;\n name = \"whatever\";\n created = 11/11/2017 12:00:00 AM;}" here is a default "PropA is 45 and PropB raise exn" and stringify "Logary.Tests.Formatting+Obj" and destructure Obj { PropB: "The property accessor threw an exception: Oh noes, no referential transparency here", PropA: 45 } Gauges: [Processor.% Idle.Core 1: 75 %, svc1 request per second: 1.75 k, methodA took 25.00 s to execute] [a.b.c.d]
  fields:
    objDefault => "PropA is 45 and PropB raise exn"
    foo => "{id = 999;\n name = \"whatever\";\n created = 11/11/2017 12:00:00 AM;}"
    objStr => "Logary.Tests.Formatting+Obj"
    objDestr => 
      Obj {
        PropB => "The property accessor threw an exception: Oh noes, no referential transparency here"
        PropA => 45}
  gauges:
    Processor.% Idle.Core 1 => "75 %"
    svc1 request per second => "1.75 k"
    methodA => "25 s"
  others:
    UserInfo => 
      User {
        name => "whatever"
        id => 999
        created => 11/11/2017 12:00:00 AM}
    simple scalar key/value map => 
      1 => "one"
      2 => "two"
    no scalar key/value map => 
      - key => 
          - [1, "a"]
          - [2, "b"]
        value => ["hello", "world"]
      - key => 
          - [2, "2"]
        value => ["3", "4"]
    _logary.errors => 
      - 
        Exception {
          TargetSite => null
          StackTrace => null
          Source => null
          Message => "another exception"
          InnerException => null
          HelpLink => null
          HResult => -2146233088
          Data => }
      - 
        Exception {
          TargetSite => null
          StackTrace => null
          Source => null
          Message => "exception with data in it"
          InnerException => null
          HelpLink => null
          HResult => -2146233088
          Data => 
            "data 1 in exn" => 1
            "data foo in exn" => 
              User {
                name => "whatever"
                id => 999
                created => 11/11/2017 12:00:00 AM}
            - key => 
                User {
                  name => "whatever"
                  id => 999
                  created => 11/11/2017 12:00:00 AM}
              value => 
                User {
                  name => "whatever"
                  id => 999
                  created => 11/11/2017 12:00:00 AM}}
    Some Tuple With 1 two foo => 
      - 1
      - "two"
      - 
        User {
          name => "whatever"
          id => 999
          created => 11/11/2017 12:00:00 AM}
    just scalar key map => 
      "some obj" => 
        Obj {
          PropB => "The property accessor threw an exception: Oh noes, no referential transparency here"
          PropA => 45}
      "some user" => 
        User {
          name => "whatever"
          id => 999
          created => 11/11/2017 12:00:00 AM}
    no scalar list => 
      - 
        User {
          name => "whatever"
          id => 999
          created => 11/11/2017 12:00:00 AM}
      - 
        - 1
        - "two"
        - 
          User {
            name => "whatever"
            id => 999
            created => 11/11/2017 12:00:00 AM}
    scalar array => [1, 2, 3, "4", "5", 6, 11/11/2017 12:00:00 AM]
"""
       Expect.equal actual (expect.TrimStart([|'\n'|])) "formatting complex message LevelDatetimePathMessageNl"

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
