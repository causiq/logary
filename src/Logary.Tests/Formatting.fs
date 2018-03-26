module Logary.Tests.Formatting
#if INTERACTIVE
#I "bin/Release/net461"
#r "Hopac.Core"
#r "Hopac"
#r "NodaTime"
#r "Logary"
#r "Expecto"
#r "Expecto.FsCheck"
#endif

open Expecto
open Expecto.Flip
open FsCheck
open Logary
open Logary.Formatting
open Logary.MessageTemplates
open Logary.MessageWriter
open Logary.Internals.Chiron
open System

#nowarn "44"

let private sampleMessage: Message =
  Message.eventFormat (Info, "this is bad, with {1} and {0} reverse.", "the first value", "the second value")
  |> Message.setName (PointName.ofList ["a"; "b"; "c"; "d"])
  |> Message.setNanoEpoch 3123456700L

type Countries =
  | Sweden
  | Germany

type IceCream =
  | Sorbé of dayTemperature: float * country: Countries
  | SoyBased of areYouVegan: bool
  | CreamBased of decilitres: float

type User =
  { id: int
    name: string
    created: DateTime }

type Obj() =
  member __.PropA =
    45
  member __.PropB =
    raise (Exception ("Oh noes, no referential transparency here"))
with
  interface IFormattable with
    member __.ToString (format, provider) = "PropA is 45 and PropB raise exn"

let date20171111 =  DateTime.Parse("2017-11-11")
let foo () = { id = 999; name = "whatever"; created = date20171111}

let complexMessage: Message =
  let ex = exn "exception with data in it"
  ex.Data.Add ("data 1 in exn", 1)
  ex.Data.Add ("data foo in exn", foo ())
  ex.Data.Add (foo(), foo())

  let tp () = (1, "two", foo())
  let (scalarArr: obj[]) = [| 1;  2; 3; "4"; "5"; 6.0; |]
  let (notScalarList: obj list) = [foo (); tp ()]
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
    | _ ->
      false

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
let shouldHaveFields msg fields message =
  msg
  |> Message.getAllFields
  |> Seq.map KV
  |> Set.ofSeq
  |> Expect.equal message (Set.ofList fields)

// just for test convenient, since file end of line is LF.
let levelDatetimeMessagePathNewLine =
  expanded false "\n" "\n"

type ProjectionTestOnly =
  { ex: exn
    user: User }

type ProjectionTestExcept =
  { user: User }

type CustomCycleReferenceRecord =
  { mutable inner: CustomCycleReferenceRecord option
    a: int
    b: string }

type CustomCycleReferenceType (id: int, name: string) =
  member val Inner =  Unchecked.defaultof<CustomCycleReferenceType> with get,set
  member __.Id = id
  member __.Name = name

let jsonRawInput = """
{"EventReceivedTime":"2018-03-19 15:33:43","SourceModuleName":"webapi","SourceModuleType":"im_file","date":"2018-03-19","time":"15:33:40","siteName":"W3SVC3060","hostName":"webfront-01","serverIp":"127.0.0.1","method":"GET","path":"/marketing/startpageconfiguration","query":"date=2018-03-19T15%3A33%3A41.0226690%2B00%3A00","listenPort":3060,"username":null,"clientIp":"127.0.0.1","protocol":"HTTP/1.1","userAgent":"GoogleHC/1.0","cookie":null,"referrer":null,"host":"localhost:3060","status":200,"substatus":0,"win32Status":0,"sent[bytes]":5028,"received[bytes]":456,"duration[ms]":3,"xForwardedFor":null,"timestamp":"2018-03-19T15:33:40Z","site":"webapi"}
"""

let testEncode<'a> fsCheckConfig =
  testPropertyWithConfig fsCheckConfig typeof<'a>.Name (fun (a: 'a) -> Json.encode a |> ignore)

module Expect =
  module Json =
    /// Assert and pass through the value.
    let isObjectX message (value: Json) =
      match value with
      | Json.Object nested ->
        nested
      | other ->
        failtestf "Expected Json.Object, but was %A" other

    /// Assert the Json value is a Json.Object.
    let isObject message value =
      isObjectX message value |> ignore

    /// Assert and pass through the found field.
    let hasFieldX message field (value: JsonObject) =
      value
        |> JsonObject.toMap
        |> Map.tryFind field
        |> function
        | None ->
          failtestf "Did not find field '%s' on Json.Object (%A)" field value
        | Some f ->
          f

    /// Assert and pass through the JsonObject value.
    let hasFieldXX message field (value: JsonObject) =
      hasFieldX message field value |> ignore
      value

    /// Assert the object has a field of the given name.
    let hasField message field value =
      hasFieldX message field value |> ignore

let jsonTests fsc =
  testList "json" [
    testList "encoding" [
      testList "primitives" [
        testEncode<uint16> fsc
        testEncode<uint32> fsc
        testEncode<uint64> fsc
        testEncode<int16> fsc
        testEncode<int32> fsc
        testEncode<int64> fsc
        testEncode<string> fsc
        testEncode<DateTime> fsc
        testEncode<DateTimeOffset> fsc
        testEncode<NodaTime.Instant> fsc
        testEncode<NodaTime.Duration> fsc
        testEncode<Gauge> fsc
        testEncode<int * int> fsc
        testEncode<Map<string, _> * Map<string, _> * string> fsc
        //testEncode<Exception> fsc
        testCase "null" (fun () ->
          Json.encode null
            |> Expect.equal "Should be Json.Null" Json.Null)
        testCase "unit" (fun () ->
          Json.encode ()
            |> Expect.equal "Should be Json.Null" Json.Null)
        testCase "None" (fun () ->
          Json.encode None
            |> Expect.equal "Should be Json.Null" Json.Null)
        testEncode<Map<string, _>> fsc
        testEncode<HashMap<string, _>> fsc
        testEncode<IceCream> fsc
      ]

      testList "nested" [
        testPropertyWithConfig fsc "Message" <| fun (m: Message) ->
          Json.encode m
            |> Expect.Json.isObjectX "The message is encoded as a Json.Object"
            |> Expect.Json.hasFieldXX "Has name field" "name"
            |> Expect.Json.hasFieldXX "Has level field" "level"
            |> Expect.Json.hasField "Has context field" "context"

        testPropertyWithConfig fsc "Exception" <| fun (e: Exception) ->
          Json.encode e
            |> Expect.Json.isObject "Returns an object"

        testCase "complex Message" <| fun () ->
          Json.encode complexMessage
            |> Expect.Json.isObject "Returns an object"
      ]
    ]

    testList "decoding" [
      testCase "message" <| fun () ->
        match Json.parse jsonRawInput |> JsonResult.bind Json.decodeMessage with
        | JPass m ->
          DateTimeOffset.ofEpoch m.timestamp
            |> Expect.equal "Should have timestamp from 'timestamp' prop in JSON"
                            (DateTimeOffset.Parse("2018-03-19T15:33:40Z"))
        | JFail err ->
          failtestf "Failed with error %A" err

      testCase "ISO8601" <| fun () ->
        match Json.parse "\"2018-08-01T01:23:45Z\"" |> JsonResult.bind Json.Decode.dateTimeOffset with
        | JPass m ->
          DateTimeOffset.ofEpoch m.timestamp
            |> Expect.equal "Parses to the right date time offset"
                            (DateTimeOffset.Parse("2018-08-01T01:23:45Z"))
        | JFail f ->
          failtestf "Failure parsing ISO8601 %A" f
    ]
  ]

let textPrinters =
  testList "text printers" [
    testCase "cycle reference" <| fun _ ->
      Message.eventFormat(Info,"cycle reference")
      |> Message.setNanoEpoch 3123456700L
      |> Message.setContext "CurrentPrincipal" System.Threading.Thread.CurrentPrincipal
      |> levelDatetimeMessagePathNewLine.format
      |> ignore // cycle reference should be handled, otherwise will throw stackoverflow exception

    ptestCase "user custom destructure resolver support cycle reference check" <| fun _ ->
      Logary.Configuration.Config.configDestructure<CustomCycleReferenceRecord>(fun resolver req ->
        let instance = req.Value
        let refCount = req.IdManager
        match refCount.TryShowAsRefId instance with
        | _, Some pv -> pv
        | refId, None ->
          let typeTag = instance.GetType().Name
          let nvs = [
            yield { Name = "Id"; Value = ScalarValue instance.a }
            yield { Name = "Name"; Value = ScalarValue instance.b }
            yield { Name = "Inner"; Value = req.WithNewValue(instance.inner) |> resolver }
          ]
          StructureValue (refId, typeTag, nvs)
        )

      let data = {inner = None; a= 42; b = "bad structure"}
      data.inner <- Some data
      Message.eventFormat(Info,"cycle reference")
      |> Message.setNanoEpoch 3123456700L
      |> Message.setContext "SelfReferenceData" data
      |> levelDatetimeMessagePathNewLine.format
      |> fun actual ->
         let expect = """
  I 1970-01-01T00:00:03.1234567+00:00: cycle reference []
    others:
      SelfReferenceData => $1
        CustomCycleReferenceRecord {
          Id => 42
          Name => "bad structure"
          Inner =>
            "Some" => $1 }
  """
         actual
           |> Expect.equal "cycle reference should work" (expect.TrimStart([|'\n'|]))

    ptestCase "projection only" <| fun _ ->
      let only = <@@ Destructure.only<ProjectionTestOnly>(fun foo ->
        [|
          foo.user.created.Day;
          foo.ex.Message;
          foo.ex.StackTrace;
          foo.ex.Data.Count;
          foo.ex.InnerException.Message
        |]) @@>

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
         actual
          |> Expect.equal "formatting the message LevelDatetimePathMessageNl with projection"
                          (expect.TrimStart([|'\n'|]))


    ptestCase "projection except" <| fun _ ->
      let except = <@@  Destructure.except<ProjectionTestExcept>(fun t -> [|t.user.created.Date|]) @@>
      let invalid = <@@ 1 + 1 @@>
      Logary.Configuration.Config.configProjection except
      Logary.Configuration.Config.configProjection invalid

      sampleMessage
      |> Message.setContext "except" { user= (foo ())}
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
         actual
            |> Expect.equal "formatting the message LevelDatetimePathMessageNl with projection"
                           (expect.TrimStart([|'\n'|]))


    testCase "StringFormatter.Verbatim" <| fun _ ->
      Message.eventError "hello world"
      |> MessageWriter.verbatim.format
      |> Expect.equal "formatting the message verbatim" "hello world"

    testCase "StringFormatter.VerbatimNewline" <| fun _ ->
      Message.eventError "hi there"
      |> MessageWriter.verbatimNewLine.format
      |> Expect.equal "formatting the message verbatim with newline" (sprintf "hi there%s" Environment.NewLine)

    testCase "StringFormatter.VerbatimNewlineTemplated" <| fun _ ->
      Message.eventFormat (Info, "what's {@direction}? {up:l}!", "up","up")
      |> MessageWriter.verbatimNewLine.format
      |> Expect.equal "formatting the message verbatim with newline, templated" (sprintf "what's \"up\"? up!%s" Environment.NewLine)

    testCase "StringFormatter.levelDatetimeMessagePathNewLine no exception" <| fun _ ->
      let expected = """I 1970-01-01T00:00:03.1234567+00:00: this is bad, with "the second value" and "the first value" reverse. [a.b.c.d]
    fields:
      0 => "the first value"
      1 => "the second value"
"""
      sampleMessage
      |> levelDatetimeMessagePathNewLine.format
      |> Expect.equal "formatting the message LevelDatetimePathMessageNl" expected

    testCase "Formatting.templateFormat, simple case" <| fun _ ->
      let format = "This {0} contains {1} words."
      let args: obj[] = [|"sentence"; 4|]
      let msg = Message.templateFormat(format, args)
      shouldHaveFields msg [KV("0","sentence"); KV("1",4)] "converting a String.Format into a message template"

    testCase "Formatting.templateFormat, named and positional fields" <| fun _ ->
      let format = "This {gramaticalStructure} contains {wordCount} {0}."
      let args: obj[] = [|"sentence"; 4; "words"|]

      let msg = Message.templateFormat(format, args)
      shouldHaveFields msg [KV("gramaticalStructure","sentence"); KV("wordCount",4);KV("0","words")]
        "fields are matched left-to-right when any fields are named"

    testCase "Formatting.templateFormat, positional fields" <| fun _ ->
      let format = "Positionally - two {2} . {2} . zero {0} . {0}"
      let args: obj[] = [|0;1;2;3|]

      let msg = Message.templateFormat(format, args)
      shouldHaveFields msg [KV("0", 0); KV("2", 2);]
        "fields are matched positionally when all are numbered"

    testCase "Formatting.templateFormat, named fields" <| fun _ ->
      let format = "This {gramaticalStructure} contains {wordCount} words."
      let args: obj[] = [|"sentence"; 4|]

      let msg = Message.templateFormat(format, args)
      shouldHaveFields msg [KV("gramaticalStructure","sentence"); KV("wordCount", 4);]
        "fields are matched left-to-right in message template"

    testCase "Formatting.templateFormat, named fields, missing last" <| fun _ ->
      let format = "This {gramaticalStructure} contains {wordCount} words."
      let args: obj[] = [|"sentence"|]

      let msg = Message.templateFormat(format, args)
      shouldHaveFields msg [KV ("gramaticalStructure", "sentence")] "fields are matched left-to-right in message template"

    testCase "Formatting.templateFormat, named fields, all missing" <| fun _ ->
      let format = "This {gramaticalStructure} contains {wordCount} words."
      let args: obj[] = [||]

      let msg = Message.templateFormat(format, args)
      shouldHaveFields msg [] "fields are matched left-to-right in message template"

    testCase "templateEvent<_> reconises the '$' symbol and will call 'ToString()' on the captured value" <| fun _ ->
      let stringifyLogEvent = Message.templateEvent<Version>(Info, "Found version {$Version}")
      let version = System.Version(1,2,3,4)
      let msg = stringifyLogEvent version

      shouldHaveFields msg [KV ("Version", version)] "Should have 'Version' KV pair"

      msg
      |> MessageWriter.verbatim.format
      |> Expect.stringContains "should call tostring() on version" "1.2.3.4"

    testCase "templateEvent<_> recognises the '@' symbol and will extract the properties of the captured value" <| fun _ ->
      let structureLogEvent = Message.templateEvent<Version>(Info, "App at {@Version}")
      let version = System.Version(1,2,3,4)
      let msg = structureLogEvent version

      shouldHaveFields msg [KV ("Version", version)] "The 'Version' field should be set."

      msg
      |> MessageWriter.verbatim.format
      |> fun actual ->
         actual |> Expect.stringContains "Extracts 'Build'" "Build"
         actual |> Expect.stringContains "Extracts 'Major'" "Major"
         actual |> Expect.stringContains "Extracts 'MajorRevision'" "MajorRevision"
         actual |> Expect.stringContains "Extracts 'Minor'" "Minor"
         actual |> Expect.stringContains "Extracts 'MinorRevision'" "MinorRevision"
         actual |> Expect.stringContains "Extracts 'Revision'" "Revision"
         actual |> Expect.stringContains "Extracts 'Version'" "Version"

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
      Expect.throws "No named fields passed 1 gen par"
        (fun () -> Message.templateEvent<int> (Info, "No named fields {0}") |> ignore)

      Expect.throws "No named fields passed 2 gen pars"
                    (fun () -> Message.templateEvent<int, int> (Info, "No named fields {0} {1}") |> ignore)
      Expect.throws "No named fields passed 3 gen pars"
                    (fun () -> Message.templateEvent<int, int, int> (Info, "No named fields {0} {1} {2}") |> ignore)
      Expect.throws "No named fields passed 4 gen pars"
                    (fun () -> Message.templateEvent<int, int, int, int> (Info, "No named fields {0} {1} {2} {3}") |> ignore)

    testCase "templateEvent<_> requires exactly the same number of type args and properties in the template" <| fun _ ->
      Expect.throws "Missing one type arg"
                  (fun () -> Message.templateEvent<int> (Info, "Too many {Field1} {Field2}") |> ignore)
      Expect.throws "Missing two type args"
                    (fun () -> Message.templateEvent<int> (Info, "Too many {Field1} {Field2} {Field3}") |> ignore)
      Expect.throws "One type arg too many"
                    (fun () -> Message.templateEvent<int> (Info, "Too few") |> ignore)

      Expect.throws "Missing one type arg"
                    (fun () -> Message.templateEvent<int, int> (Info, "Too many {Field1} {Field2} {Field3}") |> ignore)
      Expect.throws "Missing two type args"
                    (fun () -> Message.templateEvent<int, int> (Info, "Too many {Field1} {Field2} {Field3} {Field4}") |> ignore)
      Expect.throws "Two type args too many"
                    (fun () -> Message.templateEvent<int, int> (Info, "Too few") |> ignore)
      Expect.throws "One type args too many"
                    (fun () -> Message.templateEvent<int, int> (Info, "Too few {Field1}") |> ignore)

      Expect.throws "One type args too few"
                    (fun () -> Message.templateEvent<int, int, int> (Info, "Too many {Field1} {Field2} {Field3} {Field4}") |> ignore)
      Expect.throws "Two type args too few"
                    (fun () -> Message.templateEvent<int, int, int> (Info, "Too many {Field1} {Field2} {Field3} {Field4} {Field5}") |> ignore)
      Expect.throws "Three type args too many"
                    (fun () -> Message.templateEvent<int, int, int> (Info, "Too few") |> ignore)
      Expect.throws "Two type args too many"
                    (fun () -> Message.templateEvent<int, int, int> (Info, "Too few {Field1}") |> ignore)
      Expect.throws "One type arg too many"
                    (fun () -> Message.templateEvent<int, int, int> (Info, "Too few {Field1} {Field2}") |> ignore)

      Expect.throws "Missing one type arg"
                    (fun () -> Message.templateEvent<int, int, int, int> (Info, "Too many {Field1} {Field2} {Field3} {Field4} {Field5}") |> ignore)
      Expect.throws "Missing two type args"
                    (fun () -> Message.templateEvent<int, int, int, int> (Info, "Too many {Field1} {Field2} {Field3} {Field4} {Field5} {Field6}") |> ignore)
      Expect.throws "Four type args too many"
                    (fun () -> Message.templateEvent<int, int, int, int> (Info, "Too few") |> ignore)
      Expect.throws "Three type args too many"
                    (fun () -> Message.templateEvent<int, int, int, int> (Info, "Too few {Field1}") |> ignore)
      Expect.throws "Two type args too many"
                    (fun () -> Message.templateEvent<int, int, int, int> (Info, "Too few {Field1} {Field2}") |> ignore)
      Expect.throws "One type arg too many"
                    (fun () -> Message.templateEvent<int, int, int, int> (Info, "Too few {Field1} {Field2} {Field3}") |> ignore)
  ]