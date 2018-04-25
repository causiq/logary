module Logary.Tests.Codecs

open System
open Expecto
open Expecto.Flip
open Logary
open Logary.Codecs
open Logary.Ingestion

let tests =
  testList "codecs" [
    testCase "plain"  <| fun () ->
      match Codec.plain (Ingested.String "hello") with
      | Result.Ok m ->
        m.value |> Expect.equal "Should have right value" "hello"
      | Result.Error err ->
        failtestf "%s" err

    testList "log4j XML" [
      testCase "log4j XML 1" <| fun () ->
        let sample = """<log4j:event logger="com.howtodoinjava.Log4jXMLLayoutExample" timestamp="1523366809000" level="INFO" thread="main">
    <log4j:message><![CDATA[Sample info message]]></log4j:message>
    <log4j:locationInfo class="com.howtodoinjava.Log4jXMLLayoutExample" method="main" file="Log4jXMLLayoutExample.java" line="15"/>
</log4j:event>"""
        match Codec.log4jXML (Ingested.String sample) with
        | Result.Ok m ->
          m.value
            |> Expect.equal "Should parse the message properly" "Sample info message"
          m.level
            |> Expect.equal "Parses Info" Info
          m.timestamp
            |> Expect.equal "Should have correct timestamp" 1523366809000000000L
          DateTimeOffset.ofEpoch m.timestamp
            |> Expect.equal "Should equal the date this test was written"
                            (DateTimeOffset.Parse("2018-04-10 13:26:49+0000"))
          m.name
            |> Expect.equal "Should parse logger name" (PointName.parse "com.howtodoinjava.Log4jXMLLayoutExample")
        | Result.Error err ->
          failtestf "%s" err

      testCase "log4j chainsaw sample" <| fun () ->
        // NDC: https://logging.apache.org/log4j/1.2/apidocs/org/apache/log4j/NDC.html
        let sample = """<log4j:event logger="first logger" level="ERROR" thread="Thread-3" timestamp="1051494121460">
  <log4j:message><![CDATA[errormsg 3]]></log4j:message>
  <log4j:NDC><![CDATA[third]]></log4j:NDC>
  <log4j:throwable><![CDATA[java.lang.Exception: someexception-third
 	at org.apache.log4j.chainsaw.Generator.run(Generator.java:94)
]]></log4j:throwable>
  <log4j:locationInfo class="org.apache.log4j.chainsaw.Generator"
method="run" file="Generator.java" line="94"/>
  <log4j:properties>
    <log4j:data name="log4jmachinename" value="windows"/>
    <log4j:data name="log4japp" value="udp-generator"/>
  </log4j:properties>
</log4j:event>"""
        match Codec.log4jXML (Ingested.String sample) with
        | Result.Ok m ->
          m.name
            |> Expect.equal "Should parse logger name" (PointName.parse "first logger")

          m.value
            |> Expect.equal "Should parse the message properly" "errormsg 3"

          m.level
            |> Expect.equal "Parses Error level" Error

          m |> Message.tryGetField "thread"
            |> Expect.equal "Thread is put as a field" (Some "Thread-3")

          m.timestamp
            |> Expect.equal "Should have correct timestamp" (1051494121460L * 1_000_000L)

          m |> Message.tryGetField "NDC"
            |> Expect.equal "NDC is put as a field" (Some "third")

          m |> Message.tryGetField "log4jmachinename"
            |> Expect.equal "Since we can't distinguish properties into either context nor fields: put them as fields"
                            (Some "windows")

          m |> Message.tryGetField "log4japp"
            |> Expect.equal "Has log4japp-field" (Some "udp-generator")

          let error: Formatting.StacktraceLine[] =
            m |> Message.tryGetField "error"
              |> Option.get

          let expected =
            "java.lang.Exception: someexception-third\n 	at org.apache.log4j.chainsaw.Generator.run(Generator.java:94)"
            |> Formatting.DotNetStacktrace.parse

          error
            |> Expect.sequenceEqual "Should have the same stacktrace parsed" expected

        | Result.Error err ->
          failtestf "%s" err

      testCase "log4j XML bad input; no crash" <| fun () ->
        "bad bad bad"
          |> Ingested.String
          |> Codec.log4jXML
          |> Expect.isError "Failed to parse string, but did not throw."

      testCase "log4j XML no input; no crash" <| fun () ->
        ""
          |> Ingested.String
          |> Codec.log4jXML
          |> Expect.isError "Failed to parse string, but did not throw."

      testCase "log4j XML missing tags; no crash" <| fun () ->
        "<a></a>"
          |> Ingested.String
          |> Codec.log4jXML
          |> Expect.isError "Failed to parse string, but did not throw."
    ]
  ]
