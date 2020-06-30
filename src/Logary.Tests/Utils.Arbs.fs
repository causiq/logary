namespace Logary.Tests

#nowarn "25"

open System
open System.Collections.Generic
open Logary.Metric
open NodaTime
open Logary
open FsCheck
open Logary.Trace

type Arbs =
  static member Domain (): Arbitrary<Domain> =
    gen {
      let! top = ["se"; "com"; "net"] |> Gen.elements
      let! sub = [ "haf"; "qvitoo"; "bücher.ch"] |> Gen.elements
      return Domain [| sub; top |]
    }
    |> Arb.fromGen

  static member Currency(): Arbitrary<Currency> =
    let generator =
      Gen.oneof [
        Arb.generate<NonEmptyString> |> Gen.map (fun (NonEmptyString s) -> Currency.Other s)
        Gen.constant Currency.USD
        Gen.constant Currency.EUR
      ]
    Arb.fromGen generator


  static member ReadOnlyDictionary<'k, 'v when 'k : equality>(): Arbitrary<IReadOnlyDictionary<'k, 'v>> =
    Arb.Default.Dictionary()
      |> Arb.convert (fun d -> d :> IReadOnlyDictionary<_,_>) (fun x -> Dictionary<'k, 'v>(x))

  static member Uri (): Arbitrary<Uri> =
    let legalChars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-._~:#[]@!$&'()*+,;=".ToCharArray()
    let segment = gen {
      let! l = Gen.choose(1,15)
      let! chars = Gen.arrayOfLength l (Gen.elements legalChars)
      return string chars
    }
    gen {
      let! scheme = ["http"; "https"] |> Gen.elements
      let! domain = Arbs.Domain () |> Arb.toGen
      let! segs = Gen.choose(1,6)
      let! segments = Gen.listOfLength segs segment
      let path = "/" + (String.concat "/" segments)
      return Uri (sprintf "%s://%s%s" scheme domain.value path)
    }
    |> Arb.fromGen

  static member Duration() =
    Arb.Default.TimeSpan()
      |> Arb.convert (Duration.FromTimeSpan) (fun d -> d.ToTimeSpan())

  static member Map() =
    Arb.Default.Map<NonEmptyString, 'value>()

  static member Value() =
    let floats = Arb.generate<NormalFloat> |> Gen.map (fun (NormalFloat f) -> Value.Float f)
    let int64s = Arb.from<int64> |> Arb.convert Value.Int64 (function Value.Int64 ii -> ii | _ -> failwith "Not an Int64")
    let bigints = Arb.from<bigint> |> Arb.convert Value.BigInt (function Value.BigInt bi -> bi | _ -> failwith "Not a BigInt")
    let generator =
      Gen.frequency [
        6, floats
        6, int64s.Generator
        2, bigints.Generator
      ]
    let shrinker = function
      | Value.Float f -> Arb.shrink f |> Seq.map Value.Float
      | Value.Int64 _ as ii -> int64s.Shrinker ii
      | Value.BigInt _ as bi -> bigints.Shrinker bi
      | _ -> Seq.empty
    Arb.fromGenShrink (generator, shrinker)

  static member Units() =
    let isNormal f =
         not <| Double.IsInfinity f
      && not <| Double.IsNaN f
    Arb.Default.Derive()
      |> Arb.filter (function
      | U.Pow (_, n)    -> isNormal n
      | U.Offset (_, f) -> isNormal f
      | U.Scaled (_, f) -> isNormal f
      | U.Other x -> not (isNull x)
      | _ -> true)

  static member Gauge() =
    let isNormal f =
         not <| Double.IsInfinity f
      && not <| Double.IsNaN f
    Arb.Default.Derive()
      |> Arb.filter (function | Gauge (f, _) -> isNormal f.asFloat)

  static member Instant() =
    Arb.Default.DateTimeOffset()
    |> Arb.convert Instant.FromDateTimeOffset (fun i -> i.ToDateTimeOffset())

  static member StackTrace(): Arbitrary<Logary.StackTrace> =
    gen {
      return StackTrace()
    }
    |> Arb.fromGen

  static member ModuleInfo(): Arbitrary<Logary.ModuleInfo> =
    gen {
      let! (NonEmptyString m) = Arb.generate<NonEmptyString>
      let! id = Arb.generate<Id>
      return Logary.ModuleInfo(m, id.to32HexString())
    }
    |> Arb.fromGen

  static member ModelIdentifyUserMessage(): Arbitrary<Logary.Model.IdentifyUserMessage> =
    gen {
      let! prev = Arb.generate<Id>
      let! next = Arb.generate<Id>
      return Model.IdentifyUserMessage(prev.to32HexString(), next.to32HexString())
    }
    |> Arb.fromGen

  static member ModelSetUserPropertyMessage(): Arbitrary<Logary.Model.SetUserPropertyMessage> =
    gen {
      let! userId = Arb.generate<Id>
      let! NonEmptyString keyValue = Arb.generate<NonEmptyString>
      return Model.SetUserPropertyMessage(userId.toBase64String(), "browser", Value.Str keyValue)
    }
    |> Arb.fromGen

  static member ErrorInfo(): Arbitrary<Logary.ErrorInfo> =
    gen {
      let! (NonEmptyString m) = Arb.generate<NonEmptyString>
      let! (NonEmptyString t) = Arb.generate<NonEmptyString>
      let! st = Arb.generate<StackTrace>
      return ErrorInfo(m, t, st)
    }
    |> Arb.fromGen

  static member ModelHistogramMessage(): Arbitrary<Logary.Model.HistogramMessage> =
    gen {
      let registry = MetricRegistry()
      let histogramConf = HistogramConf.create("request_latencies", "An histogram specifying how long each request takes", U.Seconds)
      let genericMetric = registry.getOrCreate histogramConf
      let! labels = Arb.generate<Map<string, string>>
      let metric = genericMetric.withLabels labels

      let! len = Gen.choose(0, 15)
      let! genValues =
        Gen.arrayOfLength len Arb.generate<NormalFloat>
      let values = genValues |> Array.map (fun (NormalFloat f) -> f)
      for v in values do
        metric.observe v


      let _, MetricInfo.Histogram hi = metric.explore()
      return Model.HistogramMessage(hi)
    }
    |> Arb.fromGen

  static member ModelGaugeMessage(): Arbitrary<Logary.Model.GaugeMessage> =
    gen {
      let! g = Arb.generate<Gauge>
      let! labels = Arb.generate<Map<string, string>>
      return Model.GaugeMessage(g, labels)
    }
    |> Arb.fromGen

//  static member GaugeMessage(): Arbitrary<Logary.GaugeMessage> =
//    Arbs.ModelGaugeMessage()
//      |> Arb.convert (fun m -> m :> _) (fun m -> m :?> Model.GaugeMessage)


  static member ModelEventMessage(): Arbitrary<Logary.Model.Event> =
    let generator = gen {
      let! messageId = Arb.generate<Id>
      let! level = Arb.generate<LogLevel>
      let! parent = Gen.oneof [ Gen.constant None; Arb.generate<SpanId> |> Gen.map Some ]
      let! (NonEmptyString event) = Arb.generate<NonEmptyString>
      let! timestamp = Arb.generate<EpochNanoSeconds>
      let! name = Arb.generate<PointName>
      let! monetaryValue =
        Gen.oneof [
          Gen.constant None
          Arb.generate<NonZeroInt>
            |> Gen.map (fun (NonZeroInt i) -> money Currency.USD (float i))
            |> Gen.map Some
        ]
      let! received =
        Gen.oneof [
          Gen.constant None
          Gen.constant (timestamp + 1300L |> Some)
        ]

      let extract (NonEmptyString s, v) = s, v
      let! fields = Gen.listOf (Arb.generate<NonEmptyString * Value> |> Gen.map extract) |> Gen.map Map
      let! context = Gen.listOf (Arb.generate<NonEmptyString * Value> |> Gen.map extract) |> Gen.map Map
      let! gauges = Gen.listOf (Arb.generate<NonEmptyString * Gauge> |> Gen.map extract) |> Gen.map Map
      let! error = Arb.generate<ErrorInfo option>

      let m = Model.Event(event, monetaryValue, timestamp, messageId, name, level, context, fields, gauges,
                          ?received=received, ?error=error)
      m.parentSpanId <- parent
      return m
    }
    Arb.fromGen generator

  static member EventMessage(): Arbitrary<Logary.EventMessage> =
    Arbs.ModelEventMessage()
      |> Arb.convert (fun m -> m :> _) (fun m -> m :?> Model.Event)

//  static member SpanMessage(): Arbitrary<Logary.SpanMessage> =
//    let generator = gen {
//      let! traceId = Arb.generate<TraceId>
//      let! spanId = Arb.generate<SpanId>
//      let! parent = Gen.oneof [ Gen.constant None; Arb.generate<SpanId> |> Gen.map Some ]
//      let context = SpanContext(traceId, spanId, parent)
//      let! (NonEmptyString label) = Arb.generate<NonEmptyString>
//      let! started = Arb.generate<EpochNanoSeconds>
//      let finished = started + 1_340_000_000L // 1.34s
//      let! flags = Arb.generate<SpanFlags>
//      let! attrs = Arb.generate<Dictionary<string, Value>>
//      let! status = Arb.generate<SpanStatus>
//      let dto = Model.SpanMessage(context)
//      dto.label <- label
//      dto.started <- started
//      dto.finished <- Some finished
//      dto.spanKind <- SpanKind.Internal
//      dto.status <- status
//      dto.setAttributes attrs
//      dto.flags <- flags
//      return dto :> Logary.SpanMessage
//    }
//    // TO CONSIDER: improve shrinker
//    let shrinker (_: SpanMessage): SpanMessage seq = Seq.empty
//    Arb.fromGenShrink (generator, shrinker)

  static member Exception() =
    let failer message =
      failwith message
    let meth message =
      failer message
    let another message =
      meth message
    let generator =
      gen {
        let! (NonEmptyString message) = Arb.generate<NonEmptyString>
        let! exnFac =
          Gen.frequency [
            1, Gen.constant failer
            2, Gen.constant another
            2, Gen.constant (fun m -> ChildEx(m) :> exn)
          ]

        return
          try exnFac message
          with e -> e
      }

    let shrinker (e: exn): seq<exn> =
      if not (isNull e.InnerException) then Seq.singleton e.InnerException
      else Seq.empty

    Arb.fromGenShrink (generator, shrinker)

  static member ChildEx() =
    Arb.generate<NonEmptyString>
      |> Gen.map (fun (NonEmptyString m) -> ChildEx m)
      |> Arb.fromGen

  static member TraceState() =
    Arb.generate<(TraceStateKey * string) list>
      |> Gen.map TraceState.ofList
      |> Arb.fromGen


module Expecto =
  open Expecto
  let fsc = { FsCheckConfig.defaultConfig with arbitrary = [ typeof<Arbs> ] }
