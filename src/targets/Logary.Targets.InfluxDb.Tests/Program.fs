module Program

open Fuchu
open System
open System.IO
open Hopac
open Logary
open Logary.Targets.InfluxDb

[<Tests>]
let lineProtocol =
  testList "syntax for line protocol" [
    // measurement[,tag_key1=tag_value1...] field_key=field_value[,field_key2=field_value2] [timestamp]

    testCase "serialise timestamp" <| fun _ ->
      Assert.equal (Serialisation.serialiseTimestamp 1439587925L)
                   "1439587925"
                   "Should be identical to Logary"

    testCase "for example 1" <| fun _ ->
      let msg =
        Message.metric (PointName.ofSingle "measurement") (Float 12.)
        |> Message.setNanoEpoch 1439587925L
      let subject = Serialisation.serialiseMessage msg
      Assert.equal subject "measurement value=12 1439587925"
                   "should serialise correctly"

    testCase "for example" <| fun _ ->
      let msg =
        Object
          ([ "value", Float 12.
             "otherVal", Float 21. ] |> Map.ofList)
        |> Message.metric (PointName.ofSingle "measurement")
        |> Message.contextValue "foo" (String "bar")
        |> Message.contextValue "bat" (String "baz")
        // Timestamps must be in Unix time and are assumed to be in nanoseconds
        |> Message.setNanoEpoch 1439587925L

      let subject = Serialisation.serialiseMessage msg

      Assert.equal subject
                   "measurement,bat=baz,foo=bar otherVal=21,value=12 1439587925"
                   "should equal"
  ]

[<EntryPoint>]
let main argv = Tests.defaultMainThisAssembly argv