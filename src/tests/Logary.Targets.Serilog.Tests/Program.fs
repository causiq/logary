module Logary.Targets.Serilog.Tests

open System
open System.Net
open Expecto
open Hopac
open Logary.Targets
open Logary.Configuration
open Logary

module Target =

  /// Send the message to the target and wait for it to ack it.
  let logAndWait (target : Target.TargetInstance) message =
    Target.log target message
    |> run 
    |> run

  /// Finalise the target and assert it was finalised within 1000 milliseconds
  let finalise (target : Target.TargetInstance) =
    Alt.choose [
      Target.shutdown target
        |> Alt.afterFun Internals.Success

      timeOutMillis 1000
        |> Alt.afterFun (fun _ -> Internals.TimedOut)
    ]
    |> run
    |> function
    | Internals.TimedOut ->
      Tests.failtestf "finalising target timed out: %A" target

    | Internals.Success _ ->
      ()

let emptyRuntime =
  { Logary.Internals.RuntimeInfo.serviceName = "tests"
    Logary.Internals.RuntimeInfo.clock       = NodaTime.SystemClock.Instance
    Logary.Internals.RuntimeInfo.logger      = NullLogger() }

let createInspectableSerilogSink () =
  let recordedEvents = ResizeArray<Serilog.Events.LogEvent>()
  { new Serilog.Core.ILogEventSink
      with member this.Emit event = recordedEvents.Add event }
  , fun () -> List.ofSeq recordedEvents

let withLogaryToSerilog f =
  let inspectableSink, getSerilogEvents = createInspectableSerilogSink ()
  let serilogLogger = Serilog.LoggerConfiguration()
                        .MinimumLevel.Verbose()
                        .WriteTo.Sink(inspectableSink)
                        .CreateLogger()

  let conf = { SerilogTarget.empty with logger = serilogLogger }
  let target =  "serilog"
  let rule = Rule.createForTarget "serilog"
  let target = Target.confTarget "serilog" (SerilogTarget.create conf)
  let instance = target |> Target.init emptyRuntime |> run
  start <| instance.server (fun _ -> Job.result ()) None

  try
    f instance
  finally
    Target.finalise instance

  getSerilogEvents()

let serilogScalar value =
  (Serilog.Events.ScalarValue (box value)) :> Serilog.Events.LogEventPropertyValue

module Expect =

  type ComparableSerilogProperty =
    | Scalar of value : obj
    | Seq of items : ComparableSerilogProperty list
    | Structure of typeTag : string * items : (string * ComparableSerilogProperty) list
    | Dictionary of elements : (string * ComparableSerilogProperty) list

  /// Converts a serilog property into something we can more easily compare for equality
  let rec serilogPropertyToComparable (lepv : Serilog.Events.LogEventPropertyValue) =
    match lepv with
    | :? Serilog.Events.ScalarValue as sv -> Scalar sv.Value
    | :? Serilog.Events.SequenceValue as seqv ->
      Seq (seqv.Elements |> Seq.map serilogPropertyToComparable |> Seq.toList)
    | :? Serilog.Events.StructureValue as strv ->
      let nvps =
        strv.Properties
        |> Seq.map (fun p -> p.Name, serilogPropertyToComparable p.Value)
        |> Seq.toList
      Structure (strv.TypeTag, nvps)
    | :? Serilog.Events.DictionaryValue as dv ->
       let elems =
        dv.Elements
        |> Seq.map (fun kvp -> string kvp.Key.Value, serilogPropertyToComparable kvp.Value)
        |> Seq.toList
       Dictionary elems
    | _ -> failwithf "unknown serilog property value sub type %s" (lepv.GetType().FullName)

  let serilogPropertiesContains (event : Serilog.Events.LogEvent) (values : (string * #Serilog.Events.LogEventPropertyValue) list) =
    values
    |> Seq.iter (fun (name, expected) ->
      let exists, actualValue = event.Properties.TryGetValue name
      if not exists then
        failwithf "Expected serilog event %s to contain property %s" event.MessageTemplate.Text name
      let actualComparable, expectedComparable =
        serilogPropertyToComparable actualValue, serilogPropertyToComparable expected

      Expect.equal actualComparable expectedComparable ""
    )

[<Tests>]
let tests =
  testList "basic" [
    testCase "starting up yields no messages" <| fun _ ->
      let actualSerilogEvents = withLogaryToSerilog ignore
      Expect.equal actualSerilogEvents [] ""

    testCase "writing an event to logary calls the serilog sink" <| fun _ ->
      let logaryLogLevel, expectedLogLevel = LogLevel.Debug, Serilog.Events.LogEventLevel.Debug
      let timestampDto = DateTimeOffset.UtcNow
      let timestampInstant = NodaTime.Instant.FromDateTimeOffset timestampDto
      let templateFormat = "Hello, {who}"

      let serilogEvents = withLogaryToSerilog <| fun target ->
          Message.event logaryLogLevel "Hello, {who}"
          |> Message.setField "who" "serilog"
          |> Message.setTimestamp timestampInstant
          |> Target.logAndWait target

      match serilogEvents with
      | [] -> failwith "no serilog events found"
      | [ serilogEvent ] ->
        Expect.equal serilogEvent.Level expectedLogLevel ""
        Expect.equal serilogEvent.Exception null ""
        Expect.equal serilogEvent.Timestamp timestampDto ""
        Expect.equal serilogEvent.MessageTemplate.Text templateFormat ""
        Expect.serilogPropertiesContains serilogEvent
                                         [ "who", Serilog.Events.ScalarValue "serilog" ]

      | otherData -> failwithf "bad data %A" otherData
  ]

[<EntryPoint>]
let main args =
  Tests.runTestsInAssembly defaultConfig args
