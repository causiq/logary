// Logary target for Application Insights
module Logary.Targets.ApplicationInsights

open System
open Hopac
open Hopac.Ch
open Hopac.Infixes
open Logary
open Logary.Target
open Logary.Internals

open Microsoft.ApplicationInsights
open Microsoft.ApplicationInsights.Channel
open Microsoft.ApplicationInsights.DataContracts
open Microsoft.ApplicationInsights.DependencyCollector
open Microsoft.ApplicationInsights.Extensibility
open Microsoft.ApplicationInsights.Extensibility.Implementation

type GaugeMap =
/// Traces goes to: Overview -> Search -> Trace
| GaugeToTrace
/// Metrics goes to: Metrics Explorer -> Add Chart -> Custom
| GaugeToMetrics
/// Events goes to: Overview -> Search -> Event
| GaugeToEvent

type DerivedMap =
/// Traces goes to: Overview -> Search -> Trace
| DerivedToTrace
/// Metrics goes to: Metrics Explorer -> Add Chart -> Custom
| DerivedToMetrics
/// Events goes to: Overview -> Search -> Event
| DerivedToEvent

type EventMap =
/// Traces goes to: Overview -> Search -> Trace
| EventToTrace
/// Events goes to: Overview -> Search -> Event
| EventToEvent

type TelemetryMapping =
  {
    GaugeMapping: GaugeMap
    DerivedMapping: DerivedMap
    EventMapping: EventMap
  }

/// Microsoft Application Insights configuration
type AppInsightConf =
  { /// The Application Insights key. Get it from Azure Portal -> App Insights -> Properties -> INSTRUMENTATION KEY  https://docs.microsoft.com/azure/application-insights/app-insights-create-new-resource
    InstrumentationKey : string
    /// Whether to use Developer Mode with AI - will send more frequent messages at cost of higher CPU etc.
    DeveloperMode : bool
    /// Track external dependencies e.g. SQL, HTTP etc. etc.
    TrackDependencies : bool
    /// Map Logary types to Application Insight classes. Default: allToTrace, setting messages as Trace messages
    MappingConfiguration : TelemetryMapping
  }

let allToTrace = {GaugeMapping = GaugeToTrace; DerivedMapping = DerivedToTrace; EventMapping = EventToTrace; }
let empty = { InstrumentationKey = ""; DeveloperMode = false; TrackDependencies = false; MappingConfiguration = allToTrace }

// When creating a new target this module gives the barebones
// for the approach you may need to take.
module internal Impl =
  open System.Reflection

  let fieldValue f = 
      let (Logary.Field (v, un)) = f
      match un with
      | None -> Units.formatValue v
      | Some u -> Units.formatWithUnit Units.UnitOrientation.Prefix u v
  
  let scaleUnit = function
      | v, Scaled(u,s) -> (Value.toDouble v)/s, (Units.symbol u)
      | v, u -> Value.toDouble v, Units.symbol u

  // This is a placeholder for specific state that your target requires
  type State = { telemetryClient : TelemetryClient }

  let sdkVersion = 
      typeof<State>.GetType().Assembly.GetCustomAttributes<AssemblyInformationalVersionAttribute>()
      |> Seq.head |> fun x -> "logary: " + x.InformationalVersion
  
  let makeMetric (point,valu,uni) =
      let va, un = scaleUnit(valu,uni)
      let tel = MetricTelemetry(PointName.format point, va)
      tel.Properties.Add("Unit", un)
      tel
  
  let makeEvent (point,valu,uni) =
      let va, un = scaleUnit(valu,uni)
      let tel = EventTelemetry(PointName.format point)
      tel.Metrics.Add("item", va)
      tel.Properties.Add("Unit", un)
      tel

  // This is the main entry point of the target. It returns a Job<unit>
  // and as such doesn't have side effects until the Job is started.
  let loop (conf : AppInsightConf) // the conf is specific to your target
           (ri : RuntimeInfo) // this one,
           (requests : RingBuffer<_>) // this one, and,
           (shutdown : Hopac.Ch<_>) = // this one should always be taken in this order
    
    let rec loop (state : State) : Job<unit> =
      // Alt.choose will pick the channel/alternative that first gives a value
      Alt.choose [
        // When you get the shutdown value, you need to dispose of your resources
        // off of Hopac's execution context (see Scheduler.isolate below) and
        // then send a unit to the ack channel, to tell the requester that
        // you're done disposing.
        shutdown ^=> fun ack ->
          // do! Job.Scheduler.isolate (fun _ -> (state :> IDisposable).Dispose())
          ack *<= () :> Job<_>

        // The ring buffer will fill up with messages that you can then consume below.
        // There's a specific ring buffer for each target.
        RingBuffer.take requests ^=> function
          // The Log discriminated union case contains a message which can have
          // either an Event or a Gauge `value` property.
          | Log (message, ack) ->
            job {
              // Do something with the `message` value specific to the target
              // you are creating.

              let tel = 
                  match message.value with
                  | Gauge (valu,uni) when conf.MappingConfiguration.GaugeMapping = GaugeToMetrics ->
                      makeMetric(message.name,valu,uni) :> ITelemetry
                  | Gauge (valu,uni) when conf.MappingConfiguration.GaugeMapping = GaugeToEvent ->
                      makeEvent(message.name,valu,uni) :> ITelemetry
                  | Derived (valu,uni) when conf.MappingConfiguration.DerivedMapping = DerivedToMetrics ->
                      makeMetric(message.name,valu,uni) :> ITelemetry
                  | Derived (valu,uni) when conf.MappingConfiguration.DerivedMapping = DerivedToEvent ->
                      makeEvent(message.name,valu,uni) :> ITelemetry
                  | Event template when conf.MappingConfiguration.EventMapping = EventToEvent ->
                      EventTelemetry(template) :> ITelemetry
                  | msgName -> 
                      let loglevel = 
                          match message.level with
                          | LogLevel.Fatal -> SeverityLevel.Critical
                          | LogLevel.Error -> SeverityLevel.Error
                          | LogLevel.Warn -> SeverityLevel.Warning
                          | LogLevel.Info -> SeverityLevel.Information
                          | LogLevel.Debug -> SeverityLevel.Verbose
                          | LogLevel.Verbose -> SeverityLevel.Verbose

                      TraceTelemetry(
                          (match msgName with
                            | Gauge (v,u)
                            | Derived (v,u) -> 
                                let va, un = scaleUnit(v,u)
                                sprintf "%f %s" va un
                            | Event template -> template),
                            loglevel
                      ) :> ITelemetry

              tel.Sequence <- message.timestamp.ToString()
              tel.Timestamp <- DateTimeOffset(DateTime(1970,01,01).AddTicks(message.timestampTicks))
              tel.Context.GetInternalContext().SdkVersion <- sdkVersion
              
              match tel with
              | :? ISupportProperties as itm ->
                itm.Properties.Add("PointName", PointName.format message.name)

                message.fields |> Map.iter(fun k field ->
                  let key = PointName.format k
                  if not(itm.Properties.ContainsKey key) then
                      itm.Properties.Add(key.ToString(), (fieldValue field))
                )

                message.context |> Map.iter(fun k v ->
                  if not(itm.Properties.ContainsKey k) then
                      itm.Properties.Add(k, (Units.formatValue v))
                )
              | _ -> ()
              
              match tel with
              | :? TraceTelemetry as itm -> state.telemetryClient.TrackTrace itm
              | :? EventTelemetry as itm -> state.telemetryClient.TrackEvent itm
              | :? MetricTelemetry as itm -> state.telemetryClient.TrackMetric itm
              | x -> failwithf "Unknown telemetry item: %O" x

              // This is a simple acknowledgement using unit as the signal
              do! ack *<= ()

              return! loop { telemetryClient = state.telemetryClient }
            }

          // Since the RingBuffer is fair, when you receive the flush message, all
          // you have to do is ensure the previous Messages were successfully written
          // and then ack. Alternatively the caller can decide it's not worth the wait
          // and signal the nack, in which case you may try to abort the flush or
          // simply continue the flush in the background.
          | Flush (ackCh, nack) ->
            job {
              state.telemetryClient.Flush()

              // then perform the ack
              do! Ch.give ackCh () <|> nack

              // then continue processing messages
              return! loop { telemetryClient = state.telemetryClient }
            }
      // The target is always 'responsive', so we may commit to the alternative
      // by upcasting it to a job and returning that.
      ] :> Job<_>

    if String.IsNullOrWhiteSpace conf.InstrumentationKey then
        failwith "Azure Instrumentation key not set: InstrumentationKey. Get it from Azure Portal -> App Insights -> Properties -> INSTRUMENTATION KEY  https://docs.microsoft.com/azure/application-insights/app-insights-create-new-resource"
    else
    TelemetryConfiguration.Active.InstrumentationKey <- conf.InstrumentationKey
    TelemetryConfiguration.Active.TelemetryChannel.DeveloperMode <- Nullable conf.DeveloperMode
    if conf.TrackDependencies then
        let dependencyTracking = new DependencyTrackingTelemetryModule()
        dependencyTracking.Initialize TelemetryConfiguration.Active

    // start the inner loop by the exit of the outer loop function
    loop { telemetryClient = TelemetryClient() }

/// Create a new YOUR TARGET NAME HERE target
[<CompiledName "Create">]
let create conf name = TargetUtils.stdNamedTarget (Impl.loop conf) name

// The Builder construct is a DSL for C#-people. It's nice for them to have
// a DSL where you can't make mistakes. The general idea is that first 'new'
// is called, and you get the callback to that function. Then you can put
// methods on this Builder class which are exposed to the caller (configuration
// code).

/// Use with LogaryFactory.New( s => s.Target<YOUR TARGET NAME.Builder>() )
type Builder(conf, callParent : FactoryApi.ParentCallback<Builder>) =

  // place your own configuration methods here

  /// The Application Insights key. Get it from Azure Portal -> App Insights -> Properties -> INSTRUMENTATION KEY  https://docs.microsoft.com/azure/application-insights/app-insights-create-new-resource
  member x.SetInstrumentationKey(key : string) =
    ! (callParent <| Builder({ conf with InstrumentationKey = key }, callParent))

  /// Map Logary types to Application Insight classes. Default: allToTrace, setting messages as Trace messages
  member x.SetMappingConfiguration(telemetryMapping : TelemetryMapping) =
    ! (callParent <| Builder({ conf with MappingConfiguration = telemetryMapping }, callParent))

  /// Whether to use Developer Mode with AI - will send more frequent messages at cost of higher CPU etc.
  member x.SetDeveloperMode(developmentMode : bool) =
    ! (callParent <| Builder({ conf with DeveloperMode = developmentMode }, callParent))

  /// Track external dependencies e.g. SQL, HTTP etc. etc.
  member x.SetTrackDependencies(traceDeps : bool) =
    ! (callParent <| Builder({ conf with TrackDependencies = traceDeps }, callParent))
  // your own configuration methods end here

  // c'tor, always include this one in your code
  new(callParent : FactoryApi.ParentCallback<_>) =
    Builder(empty, callParent)

  // this is called in the end, after calling all your custom configuration
  // methods (above) which in turn take care of making the F# record that
  // is the configuration, "just so"
  interface Logary.Target.FactoryApi.SpecificTargetConf with
    member x.Build name = create conf name