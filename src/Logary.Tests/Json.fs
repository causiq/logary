module Logary.Tests.Json

open System
open Logary
open Logary.Internals.Chiron
open Logary.Formatting
open NodaTime
open Expecto
open Expecto.Flip
open NodaTime.Text

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

let date20171111 = DateTime.Parse("2017-11-11")
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
  |> Message.addGauge "svc1 request per second" (Gauge(Float 1750., Units.Scalar))
  |> Message.addGauge "Processor.% Idle.Core 1" (Gauge(Float 0.75, Units.Percent))
  |> Message.addGauge "methodA" (Gauge(Int64 25000000000L, Units.Scaled (Seconds, float Constants.NanosPerSecond)))
  |> Message.addExn ex
  |> Message.addExn (exn "another exception")


let jsonRawInput = """
{"EventReceivedTime":"2018-03-19 15:33:43","SourceModuleName":"webapi","SourceModuleType":"im_file","date":"2018-03-19","time":"15:33:40","siteName":"W3SVC3060","hostName":"webfront-01","serverIp":"127.0.0.1","method":"GET","path":"/marketing/startpageconfiguration","query":"date=2018-03-19T15%3A33%3A41.0226690%2B00%3A00","listenPort":3060,"username":null,"clientIp":"127.0.0.1","protocol":"HTTP/1.1","userAgent":"GoogleHC/1.0","cookie":null,"referrer":null,"host":"localhost:3060","status":200,"substatus":0,"win32Status":0,"sent[bytes]":5028,"received[bytes]":456,"duration[ms]":3,"xForwardedFor":null,"timestamp":"2018-03-19T15:33:40Z","site":"webapi"}
"""

let jsonInputCrash = """
{
  "context": {
    "appPackageName": "com.example",
    "appVersion": "2.2.0",
    "appVersionCode": 60,
    "brand": "google",
    "manufacturer": "Google",
    "model": "Pixel 2",
    "os": "Android",
    "osVersion": 28
  },
  "fields": {
    "requestId": "2829f296-8f8e-4905-8064-863cd02fd064",
    "responseCode": 200,
    "url": "https://api.example.com/v1/example1/command/",
    "userEmail": "person@example.com",
    "userId": "d2de3ffa-4161-4ff8-ab88-83dfd0a64b46"
  },
  "level": "Debug",
  "name": "HomeViewModel",
  "timestamp": "2019-02-20T18:03:19.42512311+01:00",
  "value": "unlock_ride_api_successful"
}
"""

let jsonBatch = """
[
  {"name":"A.B.C","value":"Changed company","fields":{},"context":{},"level":"info","timestamp":"2018-09-13T21:27:55.205Z"},
  {"name":"D.E","value":"User signed out","fields":{},"context":{},"level":"debug","timestamp":"2018-09-13T21:30:20.000Z"},
  {"name":"F.G.H","value":"User login failed 5 times","fields":{},"context":{"userId":"123456789"},"level":"warn","timestamp":1536935071394000000}
]
"""

let testEncode<'a> fsCheckConfig =
  testPropertyWithConfig fsCheckConfig typeof<'a>.Name (fun (a: 'a) -> Json.encode a |> ignore)

let ptestEncode<'a> fsCheckConfig =
  ptestPropertyWithConfig fsCheckConfig typeof<'a>.Name (fun (a: 'a) -> Json.encode a |> ignore)

let testRoundtrip<'a when 'a : equality> fsCheckConfig (decoder: JsonDecoder<'a>) =
  testPropertyWithConfig fsCheckConfig typeof<'a>.Name (fun (a: 'a) ->
    let encoded = Json.encode a
    try
      encoded
        |> decoder
        |> JsonResult.getOrThrow
        |> Expect.equal "Should eq to input" a
    with _ ->
      printfn "Encoded %A" encoded
      reraise()
    )

module Expect =
  module Message =
    let equal (message: string) (expected: Message) (actual: Message) =
      actual.name
        |> Expect.equal "name" expected.name
      actual.level
        |> Expect.equal "level" expected.level
      actual.timestamp
        |> Expect.equal "timestamp" expected.timestamp
      actual.value
        |> Expect.equal "value" expected.value

      for KeyValue (k, v) in actual.context do
        match expected.context |> HashMap.tryFind k with
        | None ->
          failtestf "Failed to find key %s in actual's Message.context" k
        | Some v ->
          v |> Expect.equal "value" v

      ()

let tests fsc =
  testList "json" [
    testCase "accessing .context" <| fun () ->
      ignore (complexMessage.context)

    testList "encoding" [
      testList "primitives" [
        testEncode<uint16> fsc
        testEncode<uint32> fsc
        testEncode<uint64> fsc
        testEncode<int16> fsc
        testEncode<int32> fsc
        testEncode<int64> fsc
        testEncode<string> fsc
        testEncode<Guid> fsc
        testEncode<Uri> fsc
        testEncode<DateTime> fsc
        testEncode<DateTimeOffset> fsc
        testEncode<TimeSpan> fsc
        testEncode<NodaTime.Instant> fsc
        testEncode<NodaTime.Duration> fsc
        testEncode<Gauge> fsc
        testEncode<int * int> fsc
        testEncode<Map<string, _> * Map<string, _> * string> fsc
        testEncode<Map<string, _>> fsc
        testEncode<HashMap<string, _>> fsc
        testEncode<Set<string>> fsc
        testEncode<IceCream> fsc
        testEncode<Collections.Generic.IDictionary<string, IceCream>> fsc

        testCase "null" (fun () ->
          Json.encode null
            |> Expect.equal "Should be Json.Null" Json.Null)

        testCase "unit" (fun () ->
          Json.encode ()
            |> Expect.equal "Should be Json.Null" Json.Null)

        testCase "None" (fun () ->
          Json.encode None
            |> Expect.equal "Should be Json.Null" Json.Null)

        testCase "LogLevel" <| fun () ->
          Logary.LogLevel.Debug
            |> Json.encode
            |> Expect.equal "Encodes to 'debug'" (String "debug")

        testCase "FSharpFunc" <| fun () ->
          let example = fun (a: int) (b: float) -> float a * b
          match Json.encode example with
          | String s ->
            match Regex.``match`` @"val example@\d{1,}: \(Int32 -> FSharpFunc`2\)" s with
            | Some g ->
              ()
            | None ->
              s |> Expect.equal "Encodes to F#-ish string"
                                "val example: int -> FSharpFunc`2"
          | x ->
            failtest "Unexpected JSON data %A" x

        testCase "PointName" <| fun () ->
          PointName [| "A"; "B" |]
            |> Json.encode
            |> Expect.equal "Encodes as array of string" (Json.Array [ Json.String "A"; Json.String "B" ])

        testCase "stacktrace" <| fun () ->
          let sample = """  at Logary.Targets.InfluxDb.Impl.extractMessage(TargetMessage request) in /logary/src/targets/Logary.Targets.InfluxDb/Targets_InfluxDb.fs:line 278
  at Microsoft.FSharp.Collections.Internal.IEnumerator.map@74.DoMoveNext(b& curr)
  at Microsoft.FSharp.Collections.Internal.IEnumerator.MapEnumerator`1.System-Collections-IEnumerator-MoveNext()
  at System.String.Join(String separator, IEnumerable`1 values)
  at Logary.Targets.InfluxDb.Impl.x2yJ@1-2(InfluxDbConf conf, RuntimeInfo ri, TargetAPI api, Uri endpoint, HttpClient client, TargetMessage[] reqs) in /logary/src/targets/Logary.Targets.InfluxDb/Targets_InfluxDb.fs:line 308
  at Hopac.Core.ContBind`2.DoWork(Worker& wr)
  at Hopac.Core.Worker.Run(Scheduler sr, Int32 me)"""

          DotNetStacktrace.parse sample
            |> Json.encode
            |> function
            | Json.Array lines ->
              lines |> Expect.isNonEmpty "Has non-empty stacktrace lines"
            | other ->
              failtestf "Unexpected json %A" other

        testCase "F# record" <| fun () ->
          { id = 1; name = "haf"; created = DateTime.UtcNow }
            |> Json.encode
            |> Expect.Json.isObjectX "Returns an object with some fields"
            |> Expect.Json.hasFieldXX "Has a name field" "name"
            |> Expect.Json.hasFieldXX "Has a created field" "created"
            |> Expect.Json.hasField "Has an id field" "id"
      ]

      testList "nested" [
        testPropertyWithConfig fsc "Exception" <| fun (e: Exception) ->
          Json.encode e
            |> Expect.Json.isObject "Returns an object"

        testCase "F# Exception" <| fun () ->
          let e = withFSharpExn id
          Json.encode e
            |> Expect.Json.isObject "Returns an object"

        testPropertyWithConfig fsc "Message" <| fun (m: Message) ->
          Json.encode m
            |> Expect.Json.isObjectX "The message is encoded as a Json.Object"
            |> Expect.Json.hasFieldXX "Has name field" "name"
            |> Expect.Json.hasFieldXX "Has level field" "level"
            |> Expect.Json.hasField "Has context field" "context"

        testCase "complex Message" <| fun () ->
          Json.encode complexMessage
            |> Expect.Json.isObjectX "Returns an object"
            |> Expect.Json.hasField "Has context field" "context"

        testPropertyWithConfig fsc "DotNetStacktrace.parse should never throw" <| fun (s: string) ->
          ignore (DotNetStacktrace.parse s)
      ]
    ]

    testList "decoding" [
      testCase "message" <| fun () ->
        match Json.parse jsonRawInput |> JsonResult.bind Json.Decode.message with
        | JPass m ->
          DateTimeOffset.ofEpoch m.timestamp
            |> Expect.equal "Should have timestamp from 'timestamp' prop in JSON"
                            (DateTimeOffset.Parse("2018-03-19T15:33:40Z"))
        | JFail err ->
          failtestf "Failed with error %A" err

      testCase "message crash (regression)" <| fun () ->
        let res =
          Json.parse jsonInputCrash
            |> JsonResult.bind Json.Decode.messageBatch
            |> JsonResult.getOrThrow
        res |> Expect.isNonEmpty "Should have message values"

      testList "offset date time" [
        yield testCase "ISO8601" <| fun () ->
          let expected = DateTimeOffset.Parse("2018-08-01T01:23:45Z")
          Json.String "2018-08-01T01:23:45Z"
            |> Json.Decode.dateTimeOffset
            |> JsonResult.getOrThrow
            |> Expect.equal "Parses to the right date time offset" expected

        yield testCase "InstantPattern.Create with nine decimals" <| fun () ->
          let p = InstantPattern.Create("yyyy-MM-dd'T'HH:mm:ss.FFFFFFFFF'Z'", Culture.invariant)
          "2019-02-01T14:45:50.123456789Z"
            |> p.Parse
            |> fun r -> r.Success
            |> Expect.isTrue "Succeeded"

        let baseI = Instant.FromUtc(2019, 02, 01, 14, 45, 50)
        let zuluSamples =
          [ "2019-02-01T14:45:50.123456789Z", 123456789L
            "2019-02-01T14:45:50.12345678Z", 123456780L
            "2019-02-01T14:45:50.1234567Z", 123456700L
            "2019-02-01T14:45:50.123456Z", 123456000L
            "2019-02-01T14:45:50.12345Z", 123450000L
            "2019-02-01T14:45:50.1234Z", 123400000L
            "2019-02-01T14:45:50.123Z", 123000000L
            "2019-02-01T14:45:50.12Z", 120000000L
            "2019-02-01T14:45:50.1Z", 100000000L
            "2019-02-01T14:45:50Z", 0L
          ]
          |> List.map (fun (i, o) -> i, baseI + Duration.FromNanoseconds o)

        let testInstant (i, e: Instant) =
          testCase (sprintf "instant %s parses correctly" i) <| fun () ->
            Json.String i
              |> Json.Decode.instant
              |> JsonResult.getOrThrow
              |> Expect.equal "Can be gotten as a Instant, accurately" e

        let testDTOZulu (i, e: Instant) =
          testCase (sprintf "date time offset %s parses correctly" i) <| fun () ->
            let e = e.ToDateTimeOffset()
            Json.String i
              |> Json.Decode.dateTimeOffset
              |> JsonResult.getOrThrow
              |> Expect.equal "Parses to the right date time offset" e

        yield! zuluSamples |> List.map testInstant
        yield! zuluSamples |> List.map testDTOZulu

        let baseO = baseI.WithOffset(Offset.FromHours 1)
        let offsetSamples =
          [ "2019-02-01T15:45:50.123456789+01:00", 123456789L
            "2019-02-01T15:45:50.123456789+01", 123456789L
            "2019-02-01T15:45:50.12345678+01:00", 123456780L
            "2019-02-01T15:45:50.1234567+01:00", 123456700L
            "2019-02-01T15:45:50.123456+01:00", 123456000L
            "2019-02-01T15:45:50.12345+01:00", 123450000L
            "2019-02-01T15:45:50.1234+01:00", 123400000L
            "2019-02-01T15:45:50.123+01:00", 123000000L
            "2019-02-01T15:45:50.12+01:00", 120000000L
            "2019-02-01T15:45:50.1+01:00", 100000000L
            "2019-02-01T15:45:50+01:00", 0L
            "2019-02-01T15:45:50+01", 0L
          ]
          |> List.map (fun (i, o) -> i, baseO + Duration.FromNanoseconds o)

        let testDTOPlus (i, o: OffsetDateTime) =
          testCase (sprintf "offset date time %s parses correctly" i) <| fun () ->
            Json.String i
              |> Json.Decode.offsetDateTime
              |> JsonResult.getOrThrow
              |> Expect.equal "Parses to the right date time offset" o

        yield! offsetSamples |> List.map testDTOPlus
      ]

      testCase "simplest possible batch JSON" <| fun () ->
        match Json.parse jsonBatch |> JsonResult.bind Json.Decode.messageBatch with
        | JPass ms ->
          let m = ms.[0]
          m.name.isEmpty
            |> Expect.isFalse "Expected non-empty name"
          m.name.ToString()
            |> Expect.equal "Should eq A.B.C." "A.B.C"
          m.value
            |> Expect.equal "Eq" "Changed company"
          m.level
            |> Expect.equal "Info level" Info

          let m = ms.[1]
          m.value
            |> Expect.equal "Eq" "User signed out"

          let m = ms.[2]
          m.timestamp
            |> Expect.equal "Has correct TS" 1_536_935_071_394_000_000L

        | JFail err ->
          failtestf "Parse failure %A" err

      testList "fields and context" [
        let sample = """{"message":"Hi {user}", "context":{"app":"native"}, "fields": {"user":"haf"}, "lastly": true, "myObj": {"isProp":"nested"} }"""
        let subject =
          Json.parse sample
            |> JsonResult.bind Json.Decode.message
            |> JsonResult.getOrThrow

        yield testCase "field" <| fun () ->
          subject |> Message.tryGetField "user" |> Expect.equal "Field equals" (Some "haf")

        yield testCase "context" <| fun () ->
          subject |> Message.tryGetContext "app" |> Expect.equal "Field equals" (Some "native")

        yield testCase "field from outside" <| fun () ->
          subject |> Message.tryGetField "lastly" |> Expect.equal "Should have a true value" (Some true)

        yield testCase "nested obj from outside" <| fun () ->
          let expected = HashMap.empty |> HashMap.add "isProp" (box "nested") |> HashMap.toList
          subject
            |> Message.tryGetField "myObj"
            |> Option.get
            |> HashMap.toList
            |> Expect.equal "Should have a true value" expected
      ]
    ]

    testList "roundtrip" [
      testRoundtrip<PointName> fsc Json.Decode.pointName
      testRoundtrip<Value> fsc Json.Decode.gaugeValue
      testRoundtrip<Gauge> fsc Json.Decode.gauge
      testRoundtrip<LogLevel> fsc Json.Decode.level

      testCase "small record" <| fun () ->
        let subject = Message.event Info "Hi" |> Message.setContext "user" (foo ())
        let encoded = subject |> Json.encode
        encoded
          |> Expect.Json.isObjectX "encodes Message to JsonObject"
          |> JsonObject.find "context"
          |> JsonResult.getOrThrow
          |> Expect.Json.isObjectX "encodes context to JsonObject"
          |> JsonObject.find "user"
          |> JsonResult.getOrThrow
          |> Expect.Json.isObject "Encodes the Record as JsonObject"

      testCase "default gauge" <| fun () ->
        let input = Message.gaugef (PointName [| "car" |]) "throttle" 0.45
        let encoded = input |> Json.encode
        let decoded = encoded |> Json.Decode.message |> JsonResult.getOrThrow
        decoded |> Expect.Message.equal "Should eq decoded" input
    ]
  ]

