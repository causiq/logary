// Logary target for Application Insights
module Logary.Targets.AppInsights

open System
open Hopac
open Hopac.Ch
open Hopac.Infixes
open Logary
open Logary.Target
open Logary.Internals

open Microsoft.ApplicationInsights
open Microsoft.ApplicationInsights.DataContracts
open Microsoft.ApplicationInsights.DependencyCollector
open Microsoft.ApplicationInsights.Extensibility
open Microsoft.ApplicationInsights.Extensibility.Implementation

/// Microsoft Application Insights configuration
type AppInsightConf =
  { /// The Application Insights key. Get it from Azure Portal -> App Insights -> Properties -> INSTRUMENTATION KEY
    AppInsightsKey : string
    /// Whether to use Developer Mode with AI - will send more frequent messages at cost of higher CPU etc.
    DeveloperMode : bool
    /// Track external dependencies e.g. SQL, HTTP etc. etc.
    TrackDependencies : bool
  }

let empty = { AppInsightsKey = ""; DeveloperMode = false; TrackDependencies = false }

// When creating a new target this module gives the barebones
// for the approach you may need to take.
module internal Impl =

  let rec serializeToString = function
    | String x -> x
    | Bool x -> x.ToString()
    | Float x -> x.ToString()
    | Int64 x -> x.ToString()
    | BigInt x -> x.ToString()
    | Binary (b,c) -> new String(System.Text.Encoding.UTF32.GetChars(b))
    | Fraction (a,b) -> a.ToString() + b.ToString()
    | Object vals -> String.Join(",", vals |> Map.map (fun k v -> k + ": " + (serializeToString v) ))
    | Array vals -> "[" + String.Join(",", vals |> List.toArray |>Array.map serializeToString) + "]"

  let fieldValue f = 
      let (Logary.Field (v, un)) = f
      (serializeToString v) + (match un with None -> "" | Some x -> " " + x.ToString())

  // This is a placeholder for specific state that your target requires
  type State = { telemetryClient : TelemetryClient }

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

              let itm = 
                TraceTelemetry(
                    (match message.value with
                     | Gauge (v,u)
                     | Derived (v,u) -> serializeToString v + " " + u.ToString()
                     | Event template -> template),
                    (match message.level with
                     | LogLevel.Fatal -> SeverityLevel.Critical
                     | LogLevel.Error -> SeverityLevel.Error
                     | LogLevel.Warn -> SeverityLevel.Warning
                     | LogLevel.Info -> SeverityLevel.Information
                     | LogLevel.Debug -> SeverityLevel.Verbose
                     | LogLevel.Verbose -> SeverityLevel.Verbose),
                     Sequence = message.timestamp.ToString(),
                     Timestamp = DateTimeOffset(DateTime(1970,01,01).AddTicks(message.timestampTicks))
                )
              
              message.fields |> Map.iter(fun k field ->
                itm.Properties.Add(k.ToString(), (fieldValue field))
              )
              
              state.telemetryClient.TrackTrace itm

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

    if String.IsNullOrWhiteSpace conf.AppInsightsKey then
        failwith "Azure Instrumentation key not set: AppInsightsKey. Get it from Azure Portal -> App Insights -> Properties -> INSTRUMENTATION KEY"
    else
    TelemetryConfiguration.Active.InstrumentationKey <- conf.AppInsightsKey
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

  /// The Application Insights key. Get it from Azure Portal -> App Insights -> Properties -> INSTRUMENTATION KEY
  member x.SetAppInsightsKey(key : string) =
    ! (callParent <| Builder({ conf with AppInsightsKey = key }, callParent))

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
