// Logary target for Application Insights
module Logary.Targets.ApplicationInsights

open System
open Hopac
open Hopac.Ch
open Hopac.Infixes
open Logary
open Logary.Target
open Logary.Internals
open Logary.Configuration

open Microsoft.ApplicationInsights
open Microsoft.ApplicationInsights.Channel
open Microsoft.ApplicationInsights.DataContracts
open Microsoft.ApplicationInsights.DependencyCollector
open Microsoft.ApplicationInsights.Extensibility
open Microsoft.ApplicationInsights.Extensibility.Implementation
open Logary.HashMap

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
    InstrumentationKey: string
    /// Whether to use Developer Mode with AI - will send more frequent messages at cost of higher CPU etc.
    DeveloperMode: bool
    /// Track external dependencies e.g. SQL, HTTP etc. etc.
    TrackDependencies: bool
    /// Map Logary types to Application Insight classes. Default: allToTrace, setting messages as Trace messages
    MappingConfiguration: TelemetryMapping
  }

let allToTrace = {GaugeMapping = GaugeToTrace; DerivedMapping = DerivedToTrace; EventMapping = EventToTrace; }
let empty = { InstrumentationKey = ""; DeveloperMode = false; TrackDependencies = false; MappingConfiguration = allToTrace }

// When creating a new target this module gives the barebones
// for the approach you may need to take.
module internal Impl =
  open System.Reflection

  // This is a placeholder for specific state that your target requires
  type State = { telemetryClient: TelemetryClient }

  let sdkVersion = 
      typeof<State>.GetType().Assembly.GetCustomAttributes<AssemblyInformationalVersionAttribute>()
      |> Seq.head |> fun x -> "logary: " + x.InformationalVersion
  
  let makeMetric (gaugeType,valu,uni) =
    let (scaled, unit) = Units.scale uni valu
    let tel = MetricTelemetry(gaugeType, scaled)
    tel.Properties.Add("Unit", unit)
    tel
  
  let makeEvent (gaugeType,valu,uni) =
    let (scaled, unit) = Units.scale uni valu
    let tel = EventTelemetry(gaugeType)
    tel.Metrics.Add("item", scaled)
    tel.Properties.Add("Unit", unit)
    tel

  let makeTrace (msg , level) =
    let loglevel = 
      match level with
      | LogLevel.Fatal -> SeverityLevel.Critical
      | LogLevel.Error -> SeverityLevel.Error
      | LogLevel.Warn -> SeverityLevel.Warning
      | LogLevel.Info -> SeverityLevel.Information
      | LogLevel.Debug -> SeverityLevel.Verbose
      | LogLevel.Verbose -> SeverityLevel.Verbose
    TraceTelemetry(msg,loglevel)


  // This is the main entry point of the target. It returns a Job<unit>
  // and as such doesn't have side effects until the Job is started.
  let loop (conf: AppInsightConf) // the conf is specific to your target
           (ri: RuntimeInfo, api: TargetAPI) =
    
    let rec loop (state: State): Job<unit> =
      // Alt.choose will pick the channel/alternative that first gives a value
      Alt.choose [
        // When you get the shutdown value, you need to dispose of your resources
        // off of Hopac's execution context (see Scheduler.isolate below) and
        // then send a unit to the ack channel, to tell the requester that
        // you're done disposing.
        api.shutdownCh ^=> fun ack ->
          // do! Job.Scheduler.isolate (fun _ -> (state :> IDisposable).Dispose())
          ack *<= () :> Job<_>

        // The ring buffer will fill up with messages that you can then consume below.
        // There's a specific ring buffer for each target.
        RingBuffer.take api.requests ^=> function
          // The Log discriminated union case contains a message which can have
          // either an Event or a Gauge `value` property.
          | Log (message, ack) ->
            job {
              // Do something with the `message` value specific to the target
              // you are creating.
              let template = message.value
              
              seq {
                let tel = 
                  match conf.MappingConfiguration.EventMapping with
                  | EventToEvent -> EventTelemetry(template) :> ITelemetry
                  | EventToTrace -> makeTrace(template,message.level) :> ITelemetry
                
                match tel with
                | :? ISupportProperties as itm ->
                  itm.Properties.Add("PointName", PointName.format message.name)
                  message.context |> HashMap.toSeq |> Seq.iter(fun (k, v)->
                    if not(itm.Properties.ContainsKey k) then
                        itm.Properties.Add(k, (string v)))
                | _ -> ()

                  
                yield tel

                yield! message 
                  |> Message.getAllGauges
                  |> Seq.map (fun (gaugeType, Gauge(value, units)) ->
                     match conf.MappingConfiguration.GaugeMapping with
                     | GaugeToMetrics -> makeMetric(gaugeType, value, units) :> ITelemetry
                     | GaugeToEvent -> makeEvent(gaugeType,value,units) :> ITelemetry
                     | GaugeToTrace -> 
                       let (scaled, unit) = Units.scale units value
                       
                       let info = sprintf "%s : %s %s" gaugeType (string scaled) (unit)
                       makeTrace(info, message.level):> ITelemetry)
              }
              |> Seq.iter (fun tel ->
                 tel.Sequence <- message.timestamp.ToString()
                 tel.Timestamp <- DateTimeOffset(DateTime(1970,01,01).AddTicks(message.timestampTicks))
                 tel.Context.GetInternalContext().SdkVersion <- sdkVersion
                 match tel with
                 | :? TraceTelemetry as itm -> state.telemetryClient.TrackTrace itm
                 | :? EventTelemetry as itm -> state.telemetryClient.TrackEvent itm
                 | :? MetricTelemetry as itm -> state.telemetryClient.TrackMetric itm
                 | x -> failwithf "Unknown telemetry item: %O" x)
              
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
              do! IVar.fill ackCh () 

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
let create conf name = TargetConf.createSimple (Impl.loop conf) name

// The Builder construct is a DSL for C#-people. It's nice for them to have
// a DSL where you can't make mistakes. The general idea is that first 'new'
// is called, and you get the callback to that function. Then you can put
// methods on this Builder class which are exposed to the caller (configuration
// code).

/// Use with LogaryFactory.New( s => s.Target<YOUR TARGET NAME.Builder>() )
type Builder(conf, callParent: Target.ParentCallback<Builder>) =

  // place your own configuration methods here

  /// The Application Insights key. Get it from Azure Portal -> App Insights -> Properties -> INSTRUMENTATION KEY  https://docs.microsoft.com/azure/application-insights/app-insights-create-new-resource
  member x.SetInstrumentationKey(key: string) =
    ! (callParent <| Builder({ conf with InstrumentationKey = key }, callParent))

  /// Map Logary types to Application Insight classes. Default: allToTrace, setting messages as Trace messages
  member x.SetMappingConfiguration(telemetryMapping: TelemetryMapping) =
    ! (callParent <| Builder({ conf with MappingConfiguration = telemetryMapping }, callParent))

  /// Whether to use Developer Mode with AI - will send more frequent messages at cost of higher CPU etc.
  member x.SetDeveloperMode(developmentMode: bool) =
    ! (callParent <| Builder({ conf with DeveloperMode = developmentMode }, callParent))

  /// Track external dependencies e.g. SQL, HTTP etc. etc.
  member x.SetTrackDependencies(traceDeps: bool) =
    ! (callParent <| Builder({ conf with TrackDependencies = traceDeps }, callParent))
  // your own configuration methods end here

  // c'tor, always include this one in your code
  new(callParent: Target.ParentCallback<_>) =
    Builder(empty, callParent)

  // this is called in the end, after calling all your custom configuration
  // methods (above) which in turn take care of making the F# record that
  // is the configuration, "just so"
  interface Target.SpecificTargetConf with
    member x.Build name = create conf name
