namespace Logary.Tests

open System
open System.Collections.Generic
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
      | otherwise -> Seq.empty
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
      |> Arb.filter (function | Gauge (f, units) -> isNormal f.asFloat)

  static member Instant() =
    Arb.Default.DateTimeOffset()
    |> Arb.convert Instant.FromDateTimeOffset (fun i -> i.ToDateTimeOffset())

  static member SpanData(): Arbitrary<SpanMessage> =
    let generator = gen {
      let! traceId = Arb.generate<TraceId>
      let! spanId = Arb.generate<SpanId>
      let! parent = Gen.oneof [ Gen.constant None; Arb.generate<SpanId> |> Gen.map Some ]
      let context = SpanContext(traceId, spanId, parent)
      let! (NonEmptyString label) = Arb.generate<NonEmptyString>
      let! started = Arb.generate<EpochNanoSeconds>
      let finished = started + 1_340_000_000L // 1.34s
      let! flags = Arb.generate<SpanFlags>
      let! attrs = Arb.generate<Dictionary<string, Value>>
      let! status = Arb.generate<SpanStatus>
      let dto = Model.SpanMessage(context)
      dto.label <- label
      dto.started <- started
      dto.finished <- Some finished
      dto.spanKind <- SpanKind.Internal
      dto.status <- status
      dto.setAttributes attrs
      dto.flags <- flags
      return dto :> Logary.SpanMessage
    }
    let shrinker (s: SpanMessage): SpanMessage seq = Seq.empty
    Arb.fromGenShrink (generator, shrinker)

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
        let! (NonEmptyString message2) = Arb.generate<NonEmptyString>
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
