namespace Logary.Trace

open System

module Constants =
  let SamplerType = "sampler.type"
  let SamplerParam = "sampler.param"
  let DefaultSamplingProbability = 0.001
  let DefaultSamplingRate = 10.0

type Sampler =
  inherit IDisposable
  abstract shouldSample: span: SpanData -> Result<SpanAttr list, unit>

namespace Logary.Trace.Sampling

open Logary
open Logary.Trace
open Constants
open Logary.Internals
open System

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
  let samplingRate =
    let r =
      samplingRate |> Option.defaultWith (fun () ->
        Env.var "JAEGER_SAMPLER_PARAM" |> Option.bind (Parse.w Double.TryParse)
      |> Option.defaultValue DefaultSamplingProbability)
    max (min r 1.0) 0.0

  let positiveBoundary, negativeBoundary =
    if abs (samplingRate - 1.0) < 0.00001 then
      Int64.MaxValue, Int64.MinValue
    else
      int64 (float Int64.MaxValue * samplingRate),
      int64 (float Int64.MinValue * samplingRate)

  let tags = [ SamplerType, SpanAttrValue.S ProbabilisticSampler.Type; SamplerParam, SpanAttrValue.V (Float samplingRate) ]
  let ok, no = Result.Ok tags, Result.Error ()

  override x.ToString() =
    sprintf "ProbabilisticSampler(samplingRate=%f, boundary=[%i, %i])" samplingRate negativeBoundary positiveBoundary

  static member Type = "probabilistic"
  interface Sampler with
    member x.shouldSample span =
      let traceId = span.context.traceId
      if traceId.isZero then no
      elif traceId.high > 0L && traceId.high <= positiveBoundary then ok
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
    member x.shouldSample span =
      if limiter.checkCredit 1.0 then ok
      else no
  interface IDisposable with
    member x.Dispose() = ()
