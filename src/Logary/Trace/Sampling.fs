namespace Logary.Trace

open System

module Constants =
  let SamplerType = "sampler.type"
  let SamplerParam = "sampler.param"
  let DefaultSamplingProbability = 0.001
  let DefaultSamplingRate = 10.0
  let DefaultKeysTracked = 50us

type Sampler =
  inherit IDisposable
  abstract shouldSample: span: SpanData -> Result<SpanAttr list, unit>

namespace Logary.Trace.Sampling

open Logary
open Logary.Message
open Logary.Internals
open Logary.Trace
open Constants
open System
open System.Collections.Generic

/// https://github.com/jaegertracing/jaeger-client-csharp/blob/master/src/Jaeger/Util/RateLimiter.cs
type RateLimiter(creditsPerSecond: float, maxBalance: float, ?getTimestamp: unit -> EpochNanoSeconds) =
  let creditsPerMillisecond = creditsPerSecond / 1000.0
  /// Returns the current timestamp as milliseconds since epoch
  let getTimestamp: unit -> int64 =
    let get = defaultArg getTimestamp Global.getTimestamp
    fun () -> get() * 1_000_000L
  let mutable balance = maxBalance
  let mutable lastTickMs = getTimestamp()

  member x.checkCredit (itemCost: float) =
    // calculate how much time passed since the last tick, and update current tick
    let currentTime = getTimestamp()
    let elapsedTime = currentTime - lastTickMs
    lastTickMs <- currentTime

    // calculate how much credit have we accumulated since the last tick
    balance <- balance + (float elapsedTime * creditsPerMillisecond)
    if balance > maxBalance then
      balance <- maxBalance

    // if we have enough credits to pay for current item, then reduce balance and allow
    if balance >= itemCost then
      balance <- balance - itemCost
      true
    else
      false

module private Parse =
  let inline w tryParse =
    fun (s: string) ->
      match tryParse s with
      | false, _ -> None
      | true, v -> Some v

[<Sealed>]
type ConstSampler(?decision: bool) =
  let decision =
    decision
      |> Option.orElseWith (fun () -> Env.varParse ((=) "true") "JAEGER_SAMPLER_PARAM")
      |> Option.defaultValue true

  let ok = Result.Ok [ SamplerType, SpanAttrValue.S ConstSampler.Type; SamplerParam, SpanAttrValue.B true ]
  override x.ToString() = sprintf "ConstSampler(%b)" decision
  static member Type = "const"
  interface Sampler with
    member x.shouldSample _ = if decision then ok else Result.Error ()
  interface IDisposable with
    member x.Dispose () = ()

[<Sealed>]
type ProbabilisticSampler(?samplingRate: float) =
  let _samplingRate =
    let r =
      samplingRate |> Option.defaultWith (fun () ->
        Env.var "JAEGER_SAMPLER_PARAM" |> Option.bind (Parse.w Double.TryParse)
      |> Option.defaultValue DefaultSamplingProbability)
    max (min r 1.0) 0.0

  let positiveBoundary, negativeBoundary =
    if abs (_samplingRate - 1.0) < 0.00001 then
      Int64.MaxValue, Int64.MinValue
    else
      int64 (float Int64.MaxValue * _samplingRate),
      int64 (float Int64.MinValue * _samplingRate)

  let tags = [ SamplerType, SpanAttrValue.S ProbabilisticSampler.Type; SamplerParam, SpanAttrValue.V (Float _samplingRate) ]
  let ok, no = Result.Ok tags, Result.Error ()

  member x.samplingRate = _samplingRate
  override x.ToString() =
    sprintf "ProbabilisticSampler(samplingRate=%f, boundary=[%i, %i))" _samplingRate negativeBoundary positiveBoundary

  static member Type = "probabilistic"
  interface Sampler with
    member x.shouldSample span =
      let traceId = span.context.traceId
      if traceId.isZero then no
      elif traceId.high > 0L && traceId.high < positiveBoundary then ok
      elif traceId.high <= 0L && traceId.high >= negativeBoundary then ok
      else no

  interface IDisposable with
    member x.Dispose() = ()

[<Sealed>]
type LevelSampler(?minLevel: LogLevel) =
  let minLevel =
    minLevel
      |> Option.orElseWith (fun () -> Env.varParse LogLevel.ofString "JAEGER_SAMPLER_PARAM")
      |> Option.defaultValue Warn

  let tags = [ SamplerType, SpanAttrValue.S LevelSampler.Type; SamplerParam, SpanAttrValue.S (minLevel.ToString()) ]
  let ok, no = Result.Ok tags, Result.Error ()
  override x.ToString() =
    sprintf "LevelSampler(minLevel=%O)" minLevel
  static member Type = "level"
  interface Sampler with
    member x.shouldSample span =
      if span.events |> Seq.exists (fun m -> m.level >= minLevel) then ok else no
  interface IDisposable with
    member x.Dispose() = ()


/// <see cref="RateLimitingSampler"/> creates a sampler that samples at most maxTracesPerSecond. The distribution of sampled
/// traces follows burstiness of the service, i.e. a service with uniformly distributed requests will have those
/// requests sampled uniformly as well, but if requests are bursty, especially sub-second, then a number of
/// sequential requests can be sampled each second.
[<Sealed>]
type RateLimitingSampler(?maxTracesPerSecond: float, ?getTimestamp) =
  let _maxRate =
    maxTracesPerSecond
      |> Option.defaultWith (fun () ->
        Env.var "JAEGER_SAMPLER_PARAM" |> Option.bind (Parse.w Double.TryParse)
      |> Option.defaultValue DefaultSamplingProbability)

  let limiter = RateLimiter(_maxRate, max _maxRate 1.0, ?getTimestamp = getTimestamp)
  let tags = [ SamplerType, SpanAttrValue.S RateLimitingSampler.Type; SamplerParam, SpanAttrValue.V (Float _maxRate) ]
  let ok, no = Result.Ok tags, Result.Error ()
  member x.maxRate = _maxRate
  override x.ToString() =
    sprintf "RateLimitingSampler(maxTracesPerSecond=%f)" _maxRate
  static member Type = "ratelimiting"
  interface Sampler with
    member x.shouldSample _ =
      if limiter.checkCredit 1.0 then ok
      else no
  interface IDisposable with
    member x.Dispose() = ()

/// <see cref="GuaranteedThroughputSampler"/> is a <see cref="Sampler"/> that guarantees a throughput by using
/// a <see cref="ProbabilisticSampler"/> and <see cref="RateLimitingSampler"/> in tandem.
///
/// The <see cref="RateLimitingSampler"/> is used to establish a lower bound rate of sampling, in that case that the
/// probabilistic sampler does not sample. However, both the probabilistic and the rate limiting samplers are updated
/// in the `shouldSample` call, so the rate limiting sampler will always count queries, even if the probabilistic
/// sampler preempts it in accepting the sample query.
///
/// Only one JAEGER_SAMPLER_PARAM can be set, and it will apply to the `probabilistic` sampler, as `tracesPerSecond` is
/// a required constructor parameter here.
[<Sealed>]
type GuaranteedThroughputSampler(tracesPerSecond: float, ?samplingRate: float, ?getTimestamp) =
  let probSampler = new ProbabilisticSampler(?samplingRate=samplingRate)
  let rateLimitingSampler = new RateLimitingSampler(tracesPerSecond, ?getTimestamp=getTimestamp)
  let sem = obj ()
  let tags decider = [
    SamplerType, SpanAttrValue.S (sprintf "%s-%s" GuaranteedThroughputSampler.Type decider)
    SamplerParam, SpanAttrValue.V (Float probSampler.samplingRate)
    "sampler.sampling_rate", SpanAttrValue.V (Float probSampler.samplingRate)
    "sampler.traces_per_second", SpanAttrValue.V (Float tracesPerSecond)
  ]
  member x.samplingRate = probSampler.samplingRate
  member x.minTracesPerSecond = tracesPerSecond
  static member Type = "guaranteed"
  override x.ToString() =
    sprintf "GuaranteedThroughputSampler(samplingRate=%f, tracesPerSecond=%f)" probSampler.samplingRate tracesPerSecond
  interface Sampler with
    member x.shouldSample span =
      let probSampler, rateLimitingSampler = probSampler :> Sampler, rateLimitingSampler :> Sampler

      lock sem <| fun () ->
      match probSampler.shouldSample span, rateLimitingSampler.shouldSample span with
      | Ok _, _ ->
        Ok (tags ProbabilisticSampler.Type)
      | _, Ok _ ->
        Ok (tags RateLimitingSampler.Type)
      | _ ->
        Result.Error ()

  interface IDisposable with
    member x.Dispose () =
      let probSampler, rateLimitingSampler = probSampler :> Sampler, rateLimitingSampler :> Sampler

      lock sem <| fun () ->
      probSampler.Dispose()
      rateLimitingSampler.Dispose()

type PerKeySamplerOptions =
  /// The target # traces per second
  | TracesPerSecond of rate: float
  /// The default sampling rate in the interval [0, 1]
  | SamplingRate of rate: float
  /// The maximum number of tracked keys
  | MaxTrackedKeys of count: uint16
  /// Optionally specify a strategy per key
  | PerKeyStrategy of key: string * strategy: Sampler

/// Computes <see cref="Sample"/> using the name of the operation, and maintains a specific
/// <see cref="GuaranteedThroughputSampler"/> instance for each operation.
[<Sealed>]
type PerKeySampler(opts: PerKeySamplerOptions list) =
  let logger = Log.create "Logary.Trace.Sampling.PerKeySampler"
  let sem = obj ()
  let keyToS = new Dictionary<string, Sampler>()
  let fallback = new ProbabilisticSampler() :> Sampler
  let mutable logged = false

  let samplingRate =
    opts
      |> List.tryPick (function SamplingRate rate -> Some rate | _ -> None)
      |> Option.defaultValue Constants.DefaultSamplingRate

  let maxTracked =
    opts
      |> List.tryPick (function MaxTrackedKeys count -> Some count | _ -> None)
      |> Option.defaultValue Constants.DefaultKeysTracked

  let createSamplerFor (label: string) =
    opts
      |> List.tryPick (function PerKeyStrategy (k, s) when k = label -> Some s | _ -> None)
      |> Option.defaultWith (fun () -> new GuaranteedThroughputSampler(samplingRate) :> Sampler)

  let findSamplerFor (span: SpanData) =
    lock sem <| fun () ->
    match keyToS.TryGetValue span.label with
    | false, _ when keyToS.Count >= int maxTracked ->
      if not logged then
        logger.info (
          eventX "Exceeded the maximum number of operations {maxTracked} for PerKeySampler"
          >> setField "maxTracked" maxTracked)
        logged <- true
      fallback

    | false, _ ->
      let sampler = createSamplerFor span.label
      keyToS.[span.label] <- sampler
      sampler

    | true, s ->
      s

  new(samplingRate, tracesPerSecond) =
    new PerKeySampler([ SamplingRate samplingRate; TracesPerSecond tracesPerSecond ])

  interface Sampler with
    member x.shouldSample span =
      let sampler = findSamplerFor span
      sampler.shouldSample span

  interface IDisposable with
    member x.Dispose() =
      for (KeyValue (_, sampler)) in keyToS do
        sampler.Dispose()
      fallback.Dispose()
