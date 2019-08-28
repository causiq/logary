module Logary.Tests.Formatting.MessageWriters

open Expecto
open Expecto.Flip
open System
open Logary
open Logary.Tests
open Logary.Formatting
open Logary.Formatting.MessageTemplates
open Logary.MessageWriter

type User =
  { id: int
    name: string
    created: DateTime }
let date20171111 = DateTime.Parse("2017-11-11")
let foo () = { id = 999; name = "whatever"; created = date20171111}

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
  expanded true true "\n" "\n"

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

let private sampleMessage: Message =
  Message.eventFormat (Info, "this is bad, with {1} and {0} reverse.", "the first value", "the second value")
  |> Message.setName (PointName.ofList ["a"; "b"; "c"; "d"])
  |> Message.setNanoEpoch 3123456700L

let tests =
  testList "message writers" [
    testCase "singleLineNoContext with exception" <| fun _ ->
      let ex = withException id
      Message.event Error "Hi"
      |> Message.setNameStr "A.B.C"
      |> Message.setNanoEpoch 3123456700L
      |> Message.addExn ex
      |> MessageWriter.singleLineNoContext.format
      |> Expect.stringContains
        "Should return correct string"
        "E 1970-01-01T00:00:03.1234567+00:00: Hi [A.B.C] System.Exception: Bad things going on   at Logary.Tests.Utils.innermost[a](Boolean throwCLRExn) in"

    testCase "cycle reference" <| fun _ ->
      Message.eventFormat(Info, "cycle reference")
      |> Message.setNanoEpoch 3123456700L
      |> Message.setContext "CurrentPrincipal" System.Threading.Thread.CurrentPrincipal
      |> levelDatetimeMessagePathNewLine.format
      |> ignore // cycle reference should be handled, otherwise will throw stackoverflow exception

    testCase "user custom destructure resolver support cycle reference check" <| fun _ ->
      Logary.Configuration.Config.destructurer<CustomCycleReferenceRecord>(fun resolver req ->
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
      let expected = """I 1970-01-01T00:00:03.1234567+00:00: cycle reference []
  others:
    SelfReferenceData => $1
      CustomCycleReferenceRecord {
        Id => 42
        Name => "bad structure"
        Inner =>
          "Some" => $1 }"""

      Message.eventFormat(Info,"cycle reference")
      |> Message.setNanoEpoch 3123456700L
      |> Message.setContext "SelfReferenceData" data
      |> levelDatetimeMessagePathNewLine.format
      |> Expect.linesEqual "Lines should equal expected" expected

    testCase "projection only" <| fun _ ->
      let expected = """I 1970-01-01T00:00:03.1234567+00:00: this is bad, with "the second value" and "the first value" reverse. [a.b.c.d]
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
      let only = <@@ Destructure.only<ProjectionTestOnly>(fun foo ->
        [|
          foo.user.created.Day;
          foo.ex.Message;
          foo.ex.StackTrace;
          foo.ex.Data.Count;
          foo.ex.InnerException.Message
        |]) @@>

      Logary.Configuration.Config.projection only

      let inner = exn "inner exception"
      let e = new Exception("top", inner)
      e.Data.Add(1,2)
      e.Data.Add(3,4)

      sampleMessage
      |> Message.setContext "only" {ex = e; user= (foo ())}
      |> levelDatetimeMessagePathNewLine.format
      |> Expect.linesEqual "formatting the message LevelDatetimePathMessageNl with projection" expected

    testCase "projection except" <| fun _ ->
      let expected = """I 1970-01-01T00:00:03.1234567+00:00: this is bad, with "the second value" and "the first value" reverse. [a.b.c.d]
  fields:
    0 => "the first value"
    1 => "the second value"
  others:
    except =>
      ProjectionTestExcept {
        user =>
          User {
            created =>
              DateTime {
                Day => 11
                DayOfWeek => "Saturday"
                DayOfYear => 315
                Hour => 0
                Kind => "Unspecified"
                Millisecond => 0
                Minute => 0
                Month => 11
                Second => 0
                Ticks => 636459552000000000
                TimeOfDay => 00:00:00
                Year => 2017}
            id => 999
            name => "whatever"}}
"""
      let except = <@@  Destructure.except<ProjectionTestExcept>(fun t -> [|t.user.created.Date|]) @@>
      let invalid = <@@ 1 + 1 @@>
      Logary.Configuration.Config.projection except
      Logary.Configuration.Config.projection invalid

      sampleMessage
      |> Message.setContext "except" { user= (foo ())}
      |> levelDatetimeMessagePathNewLine.format
      |> Expect.linesEqual "formatting the message LevelDatetimePathMessageNl with projection" expected

    testCase "MessageWriter.verbatim" <| fun _ ->
      Message.eventError "hello world"
      |> MessageWriter.verbatim.format
      |> Expect.equal "formatting the message verbatim" "hello world"

    testCase "MessageWriter.verbatimNewline 1" <| fun _ ->
      Message.eventError "hi there"
      |> MessageWriter.verbatimNewLine.format
      |> Expect.equal "formatting the message verbatim with newline" (sprintf "hi there%s" Environment.NewLine)

    testCase "MessageWriter.verbatimNewline 2" <| fun _ ->
      Message.eventFormat (Info, "what's {@direction}? {up:l}!", "up","up")
      |> MessageWriter.verbatimNewLine.format
      |> Expect.equal "formatting the message verbatim with newline, templated" (sprintf "what's \"up\"? up!%s" Environment.NewLine)

    testCase "MessageWriter.verbatimNewline with context" <| fun _ ->
      Message.event Debug "Started App {softwareVersion} in {duration}"
      |> Message.setContext "softwareVersion" "v1.2.3-a4a4a4"
      |> Message.setField "duration" "1.151s"
      |> MessageWriter.verbatimNewLine.format
      |> Expect.equal "Should format from context too"
                      (sprintf "Started App \"v1.2.3-a4a4a4\" in \"1.151s\"%s" Environment.NewLine)

    testCase "MessageWriter.levelDatetimeMessagePathNewLine no exception" <| fun _ ->
      let expected = """I 1970-01-01T00:00:03.1234567+00:00: this is bad, with "the second value" and "the first value" reverse. [a.b.c.d]
    fields:
      0 => "the first value"
      1 => "the second value" """

      sampleMessage
      |> levelDatetimeMessagePathNewLine.format
      |> Expect.linesEqual "formatting the message LevelDatetimePathMessageNl" expected

    testCase "Message.eventFormat, simple case" <| fun _ ->
      let format = "This {0} contains {1} words."
      let args: obj[] = [|"sentence"; 4|]
      let msg = Message.eventFormat(format, args)
      shouldHaveFields msg [KV("0","sentence"); KV("1",4)] "converting a String.Format into a message template"

    testCase "Message.eventFormat, named and positional fields" <| fun _ ->
      let format = "This {grammaticalStructure} contains {wordCount} {0}."
      let args: obj[] = [|"sentence"; 4; "words"|]

      let msg = Message.eventFormat(format, args)
      shouldHaveFields msg [KV("grammaticalStructure","sentence"); KV("wordCount",4);KV("0","words")]
        "fields are matched left-to-right when any fields are named"

    testCase "Message.eventFormat, positional fields" <| fun _ ->
      let format = "Positionally - two {2} . {2} . zero {0} . {0}"
      let args: obj[] = [|0;1;2;3|]

      let msg = Message.eventFormat(format, args)
      shouldHaveFields msg [KV("0", 0); KV("2", 2);]
        "fields are matched positionally when all are numbered"

    testCase "eventFormat, named fields" <| fun _ ->
      let format = "This {grammaticalStructure} contains {wordCount} words."
      let args: obj[] = [|"sentence"; 4|]

      let msg = Message.eventFormat(format, args)
      shouldHaveFields msg [KV("grammaticalStructure","sentence"); KV("wordCount", 4);]
        "fields are matched left-to-right in message template"

    testCase "eventFormat, named fields, missing last" <| fun _ ->
      let format = "This {grammaticalStructure} contains {wordCount} words."
      let args: obj[] = [|"sentence"|]

      let msg = Message.eventFormat(format, args)
      shouldHaveFields msg [KV ("grammaticalStructure", "sentence")] "fields are matched left-to-right in message template"

    testCase "eventFormat, named fields, all missing" <| fun _ ->
      let format = "This {grammaticalStructure} contains {wordCount} words."
      let args: obj[] = [||]

      let msg = Message.eventFormat(format, args)
      shouldHaveFields msg [] "fields are matched left-to-right in message template"

    testCase "templateEvent<_> recognises the '$' symbol and will call 'ToString()' on the captured value" <| fun _ ->
      let stringifyLogEvent = Message.templateEvent<Version>(Info, "Found version {$Version}")
      let version = System.Version(1,2,3,4)
      let msg = stringifyLogEvent version

      shouldHaveFields msg [KV ("Version", version)] "Should have 'Version' KV pair"

      msg
      |> MessageWriter.verbatim.format
      |> Expect.stringContains "should call ToString() on version" "1.2.3.4"

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

let stacktrace fsc =
  testList "DotNetStacktrace" [
    testCase "stacktrace 2" <| fun () ->
      let sample = """
CompanyA.WebApi.Client.WebApiException: Service Web API Error ---&gt; ServiceStack.ServiceClient.Web.WebServiceException: RestException
   at ServiceStack.ServiceClient.Web.ServiceClientBase.ThrowWebServiceException[TResponse](Exception ex, String requestUri)
   at ServiceStack.ServiceClient.Web.ServiceClientBase.ThrowResponseTypeException[TResponse](Object request, Exception ex, String requestUri)
   at ServiceStack.ServiceClient.Web.ServiceClientBase.HandleResponseException[TResponse](Exception ex, Object request, String requestUri, Func`1 createWebRequest, Func`2 getResponse, TResponse&amp; response)
   at ServiceStack.ServiceClient.Web.ServiceClientBase.Send[TResponse](String httpMethod, String relativeOrAbsoluteUrl, Object request)
   at CompanyA.WebApi.Client.ApiClient.Send[TResponse](Func`2 func, Int32 retry)
   --- End of inner exception stack trace ---
   at CompanyA.WebApi.Client.ApiClient.Send[TResponse](Func`2 func, Int32 retry)
   at CompanyA.WebApi.Client.ApiClient.Post[TResponse](IReturn`1 request)
   at CompanyA.TouchWeb.Areas.Default.Services.SellableTicketsService.ExecuteSavedSearch(CustomResultPageApiResponse response) in C:\Projects\app\Applications\Web\SellableTicketsService.cs:line 73
   at CompanyA.TouchWeb.Areas.Default.Services.SellableTicketsService.Get() in C:\Projects\app\Applications\Web\SellableTicketsService.cs:line 40"""
      let parsed = DotNetStacktrace.parse sample
      let withLines = parsed.[parsed.Length - 2..parsed.Length - 1]
      withLines.[0]
        |> function
          | Line line ->
            line.file |> Expect.equal "Should have parsed the file path" (Some @"C:\Projects\app\Applications\Web\SellableTicketsService.cs")
            line.lineNo |> Expect.equal "Should have parsed the file path" (Some 73)
          | other ->
            failtestf "Unexpected %A" other

    testCase "fusion stacktrace" <| fun () ->
      let sample = """
System.IO.FileNotFoundException: Could not load file or assembly 'Google.Api.Gax.Rest, Version=2.2.1.0, Culture=neutral, PublicKeyToken=3ec5ea7f18953e47' or one of its dependencies. The system cannot find the file specified.
File name: 'Google.Api.Gax.Rest, Version=2.2.1.0, Culture=neutral, PublicKeyToken=3ec5ea7f18953e47'
   at Google.Cloud.Storage.V1.StorageClient.Create(GoogleCredential credential, EncryptionKey encryptionKey)
   at A.B.C.D.WebDav.WebDavService.<>c.<.ctor>b__4_0() in C:\A\B\C\D\WebDav\WebDavService.cs:line 27
   at System.Lazy`1.CreateValue()
   at System.Lazy`1.LazyInitValue()
   at A.B.C.D.WebDav.WebDavService.TryGetFileSizeGCloud(String url, Int64& fileSize) in C:\A\B\C\D\WebDav\WebDavService.cs:line 119

WRN: Assembly binding logging is turned OFF.
To enable assembly bind failure logging, set the registry value [HKLM\Software\Microsoft\Fusion!EnableLog] (DWORD) to 1.
Note: There is some performance penalty associated with assembly bind failure logging.
To turn this feature off, remove the registry value [HKLM\Software\Microsoft\Fusion!EnableLog].
"""
      let parsed = DotNetStacktrace.parse sample

      parsed.[0] |> function
        | ExnType (et, msg) ->
          et |> Expect.equal "Should be 'FileNotFoundException'" "System.IO.FileNotFoundException"
          msg |> Expect.equal "Should be the remainder" "Could not load file or assembly 'Google.Api.Gax.Rest, Version=2.2.1.0, Culture=neutral, PublicKeyToken=3ec5ea7f18953e47' or one of its dependencies. The system cannot find the file specified."
        | other -> failtestf "Unexpected %A" other

      parsed.[1] |> function
        | LineOutput msg ->
          msg |> Expect.equal "Should be the full line" "File name: 'Google.Api.Gax.Rest, Version=2.2.1.0, Culture=neutral, PublicKeyToken=3ec5ea7f18953e47'"
        | other -> failtestf "Unexpected %A" other

      parsed.[7] |> function
        | LineOutput msg ->
          msg |> Expect.equal "Should be the full line" "WRN: Assembly binding logging is turned OFF."
        | other -> failtestf "Unexpected %A" other

      parsed.[8] |> function
        | LineOutput msg ->
          msg |> Expect.equal "Should be the full line" "To enable assembly bind failure logging, set the registry value [HKLM\Software\Microsoft\Fusion!EnableLog] (DWORD) to 1."
        | other -> failtestf "Unexpected %A" other

    testCase "wcf stacktrace" <| fun () ->
      let sample = """System.ServiceModel.FaultException`1[System.ServiceModel.ExceptionDetail]: One or more errors occurred. (Fault Detail is equal to An ExceptionDetail, likely created by IncludeExceptionDetailInFaults=true, whose value is:
System.AggregateException: One or more errors occurred. ----> System.IndexOutOfRangeException: AKeyHere
   at System.Data.ProviderBase.FieldNameLookup.GetOrdinal(String fieldName)
   at System.Data.SqlClient.SqlDataReader.GetOrdinal(String name)
   at DatabaseAccess.SqlReaderExtensions.GetDefault[T](SqlDataReader r, String key) in d:\BuildAgent\work\ae3e9a35f63a8b8f\src\DatabaseAccess\DatabaseAccess\SqlReaderExtensions.cs:line 56
   at ProcessServices.MemberQuery.<>c.<GetMember>b__2_1(SqlDataReader r) in C:\dev\DatabaseQueries\MemberQuery.cs:line 23
   at DatabaseAccess.DbAccessor.<>c__DisplayClass14`1.<PerformSpReadSingle>b__13(SqlDataReader reader) in d:\BuildAgent\work\ae3e9a35f63a8b8f\src\DatabaseAccess\DatabaseAccess\DbAccessor.cs:line 284"""

      let parsed = DotNetStacktrace.parse sample

      parsed.[0] |> function
        | ExnType (et, msg) ->
          et |> Expect.equal "Should equal 'FaultException'" "System.ServiceModel.FaultException`1[System.ServiceModel.ExceptionDetail]"
          msg |> Expect.equal "Should have message" "One or more errors occurred. (Fault Detail is equal to An ExceptionDetail, likely created by IncludeExceptionDetailInFaults=true, whose value is:"
        | other -> failtestf "Unexpected %A" other

      parsed.[1] |> function
        | ExnType (et, msg) ->
          et |> Expect.equal "Should equal 'AggregateException'" "System.AggregateException"
          msg |> Expect.equal "Should have message" "One or more errors occurred. ----> System.IndexOutOfRangeException: AKeyHere"
        | other -> failtestf "Unexpected %A" other
  ]