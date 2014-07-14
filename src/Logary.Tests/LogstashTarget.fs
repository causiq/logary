module Logary.Tests.LogstashTarget

open System

open Fuchu

open Logary
open Logary.Target
open Logary.Targets
open Logary.Internals
open Logary.Internals.Tcp

open Logary.Tests.Fac
open Logary.Tests.TestDSL

let raised_exn msg =
  let e = ref None : exn option ref
  try raise <| ApplicationException(msg)
  with ex -> e := Some ex
  (!e).Value

[<Tests>]
let tests =
  testList "logstash target" [
    testCase "plain logging" <| fun _ ->
      let target = Logstash.create (Logstash.LogstashConf.Create("10.0.0.120", 1936us, StubTcp.StubWriterClient.Create)) "logstash-integration"
      let subject = target |> initTarget { serviceName = "tests"; logger = NullLogger() }
      (because "logging warning to logstash" <| fun () ->
        LogLine.warnTag "integration" "integration test" |> logTarget subject
        subject |> finaliseTarget
        ())
      |> thatsIt

    testCase "``when logging into logstash should output version one event``" <| fun _ ->
      let e1 = raised_exn "darn"
      let e2 = raised_exn "actual exn"
      // pipe a to onto f and g
      let (-|>) a (f, g) = f a, g a
      let writer  = new StubTcp.StubWriterClient(true)
      let conf    = Logstash.LogstashConf.Create("127.0.0.1", clientFac = fun _ _ -> writer :> WriteClient)
      let target  = Logstash.create conf "logstash-integration"
      let subject = target |> initTarget { serviceName = "tests"; logger = NullLogger() }
      let msg     = "integration test message"
      (because "logging warning to logstash" <| fun () ->
        LogLine.warnTag "integration" msg
        |> LogLine.setPath "a.b.c"
        |> LogLine.setData "data-key" "data-value"
        |> LogLine.setData "e" e1
        |> LogLine.setExn e2
        |> logTarget subject
        subject |> finaliseTarget
        writer.ReadLines() |> Seq.exactlyOne |> Newtonsoft.Json.Linq.JObject.Parse)
      |> should' (fulfil (fun t -> "timestamp should not be null", not(t.["timestamp"] = null)))
      |> should' (fulfil (fun t -> "@version should not be null", not(t.["@version"] = null)))
      |> should' (fulfil (fun t -> "message should not be null", not(t.["message"] = null)))
      |> should' (fulfil (fun t -> "path should not be null", not(t.["path"] = null)))
      |> should' (fulfil (fun t -> "hostname should not be null", not(t.["hostname"] = null)))
      |> should' (fulfil (fun t -> "service should not be null", not(t.["service"] = null)))
      |> should' (fulfil (fun t -> "level should not be null", not(t.["level"] = null)))
      |> should' (fulfil (fun t -> "data-key should not be null", not(t.["data-key"] = null)))
      |> should' (fulfil (fun t -> "e should not be null", not(t.["e"] = null)))
      |> should' (fulfil (fun t -> "exception should not be null", not(t.["exception"] = null)))

      |> should' (fulfil (fun t ->
          (Net.Dns.GetHostName()) -|> (sprintf "hostname should be %s", (=) (string t.["hostname"]))))
      |> should' (fulfil (fun t -> "should have same message", string t.["message"] = msg))
      |> should' (fulfil (fun t -> "should have correct level", string t.["level"] = "warn"))
      |> should' (fulfil (fun t -> sprintf "service, expected 'tests', but was '%s'" (string t.["service"]),
                                    string t.["service"] = "tests"))
      |> should' (fulfil (fun t -> "should have correct path", string t.["path"] = "a.b.c"))
      // and data keys/values should be on the root of the object:
      |> should' (fulfil (fun t -> "should contain data-key", string t.["data-key"] = "data-value"))
      |> should' (fulfil (fun t -> "should contain 'integration' tag", string t.["tags"].[0] = "integration"))
      |> should' (fulfil (fun t -> "should contain 'e' serialized value", string t.["e"].["Message"] = "darn"))
      |> should' (fulfil (fun t -> "should contain 'exception' serialized value", string t.["exception"].["Message"] = "actual exn"))
      |> thatsIt

    ]