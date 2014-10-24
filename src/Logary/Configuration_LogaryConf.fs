namespace Logary.Configuration

open System.Globalization
open System.IO

open Newtonsoft.Json

open NodaTime

open FSharp.Actor

open Logary
open Logary.Formatting
open Logary.Internals
open Logary.Target
open Logary.Metric

/// The logary configuration structure having a memory of all configured
/// targets as well as the rules that map log lines and metrics to them.
type LogaryConf =
  { /// A list of rules that guide what targets are invoked for a given
    /// log line or measure.
    rules    : Rule list

    /// A map of the targets by name. Some targets may not have been initialised
    /// (LogaryConf describes what-is-to-be before running it, thereafter it describes
    /// what has been configured, and the targets instances aren't None anymore.)
    targets  : Map<string, TargetConf * Option<TargetInstance>>

    /// Service metadata - what name etc.
    metadata : RuntimeInfo

    /// A map of metrics by name
    metrics  : Map<string, MetricConf * Option<IActor>>

    /// how often do we poll metrics
    pollPeriod : Duration }

module LogaryConfLenses =

  open Logary.Lenses
  open Logary.Internals

  let rules_ =
    { get = fun x -> x.rules
      set = fun v x -> { x with rules = v } }

  let targets_ =
    { get = fun x -> x.targets
      set = fun v x -> { x with targets = v } }

  let metadata_ =
    { get = fun x -> x.metadata
      set = fun v x -> { x with metadata = v } }

  let metrics_ =
    { get = fun x -> x.metrics
      set = fun v x -> { x with metrics = v } }

  /// read all configuration's actors
  let targetActors_ =
    { get = fun (x : Map<_, _>) -> x |> Seq.map (fun kv -> kv.Value |> snd |> Option.get |> fun x -> x.actor)
      set = fun _ _ -> failwith "cannot set target actors" }

  /// read and write a very specific target actor
  let targetActor_ name =
    { get = fun x -> x.targets |> Map.find name |> fun (_, minst) -> minst |> Option.get
      set = fun v x ->
        let value' = x.targets |> Map.find name |> fst, Some v
        { x with targets = x.targets |> Map.put name value' } }

  /// rad and write a very specific metric actor
  let metricActor_ name =
    { get = fun x -> x.metrics |> Map.find name |> (snd >> Option.get)
      set = fun v x ->
        let value' = x.metrics |> Map.find name |> fst, Some v
        { x with metrics = x.metrics |> Map.put name value' } }

/// DTOs used to reify a configuration from text
module DTOs =

  type RuleDto =
    { hiera       : string
      target      : string
      level       : LogLevel }

  type TargetDto =
    { name        : string
      ``module``  : string }

  type MetricDto =
    { name        : string
      ``module``  : string }

  type ConfDto =
    { rules       : RuleDto list
      targets     : TargetDto list
      metrics     : MetricDto list
      serviceName : string
      pollPeriod  : Duration }

  let writeJson (writer : TextWriter) (dto : ConfDto) =
    let opts = JsonFormatter.Settings ()
    let serialiser = JsonSerializer.Create opts
    serialiser.Serialize(writer, dto)

  let writeJson' (dto : ConfDto) =
    use sw = new StringWriter(CultureInfo.InvariantCulture)
    writeJson sw dto
    sw.ToString()

  let readJson (reader : TextReader) =
    let opts = JsonFormatter.Settings ()
    let serialiser = JsonSerializer.Create opts
    serialiser.Deserialize(reader, typeof<ConfDto>)
    :?> ConfDto

  let readJson' (conf : string) =
    use sr = new StringReader(conf)
    readJson sr
