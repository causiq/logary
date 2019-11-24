namespace Logary.Trace.Propagation

open System
open System.Globalization
open System.Text
open System.Text.RegularExpressions
open Logary.Trace
open Logary.Internals.Regex
open Logary.YoLo

module internal EdgeCaseLogic =

  type Signature = uint8 * TraceId * SpanId -> uint8 * TraceId * SpanId

  let zeroSpanShouldRegenerateBoth: Signature =
    function
    | v, _, spanId when spanId.isZero ->
      v, TraceId.create(), SpanId.create()
    | v, traceId, spanId -> v, traceId, spanId

  let zeroTraceShouldRegenerateBoth: Signature =
    function
    | v, traceId, _ when traceId.isZero ->
      v, TraceId.create(), SpanId.create()
    | v, traceId, spanId -> v, traceId, spanId

  let test_traceparent_version_0xff: Signature =
    function
    | v, t, s when v <> 0uy ->
      0uy, TraceId.create(), SpanId.create()
    | v, t, s ->
      v, t, s

  let apply: Signature =
    test_traceparent_version_0xff
    >> zeroSpanShouldRegenerateBoth
    >> zeroTraceShouldRegenerateBoth

  let applyWithFlags value flags =
    let v, t, s = apply value
    t, s, flags

/// https://w3c.github.io/trace-context/#trace-context-http-headers-format
module W3C =
  open Extract

  let TraceParentHeader = "traceparent"


  [<Struct>]
  type NameValue =
    { name: string
      value: string
      properties: (string * string option) list }
    member x.hasProperties = not (List.isEmpty x.properties)

  type CorrelationContext(nameValues: NameValue list) =
    let m = lazy (nameValues |> List.map (fun x -> x.name, x) |> Map.ofList)
    let l = lazy (m.Value |> Seq.map (fun (KeyValue (_, v)) -> v) |> List.ofSeq)
    let ms = lazy (m.Value |> Map.map (fun _ v -> v.value))
    member x.value = m.Value
    member x.valueStrings = ms.Value
    member x.asList = l.Value
    member x.isEmpty = List.isEmpty nameValues



  module CorrelationContext =
    open FParsec

    type Parser<'t> = Parser<'t, unit>

    let private toBytes: string -> byte list =
      Encoding.UTF8.GetBytes >> List.ofArray

    let private toBytePair (s: string) =
      byte >> fun i -> i, toBytes (i.ToString s)

    let private index =
      let range = [ 0x00 .. 0xff ]
      let lower = List.map (toBytePair "x2") range
      let upper = List.map (toBytePair "X2") range

      lower @ upper

    let private byteIndex =
      index
      |> Map.ofList

    let private hexIndex =
      index
      |> List.map (fun (a, b) -> (b, a))
      |> Map.ofList

    let private pctP: Parser<char[]> =
      tuple3 (pchar '%') hex hex
      |>> (fun (p, a, b) ->
          [| p; a; b |])

    let private makeParser (pred: char -> bool) =
      many (attempt pctP <|> (satisfy pred |>> Array.singleton))
      |>> fun x ->
          new string (Array.concat x)

    let Header = "correlation-context"

    let headerNameP: Parser<string> = pstringCI Header

    let nameP =
      spaces >>. makeParser ((<>) '=') .>> spaces

    let valueP: Parser<string * (string * string option) list> = // '   haf;awesome;really=yes!%F0%9F%8E%B8%0A'
      let contentsP: Parser<string> = // haf
        makeParser (fun c -> c <> ';' && c <> ',')

      let propertyOpP: Parser<string> = // 'really' | 'yes!%F0%9F%8E%B8%0A'
        makeParser (fun c -> c <> '=' && c <> ';')

      let propertyP: Parser<string * string option> = // ';awesome' | 'really=yes!%F0%9F%8E%B8%0A'
        skipChar ';' >>. (propertyOpP .>>. opt (skipChar '=' >>. propertyOpP))

      let propertiesP: Parser<(string * string option) list> = // 'awesome;oh yes' | 'a;b;c=e'
        sepEndBy1 propertyP (pchar ';')

      let optPropertiesP: Parser<(string * string option) list> = // same as above, but yields empty list if non-existent
        opt propertiesP |>> Option.defaultValue []

      spaces >>. contentsP .>>. optPropertiesP .>> spaces

    let headerValueP: Parser<CorrelationContext> =
      let nameValueP =
        nameP .>>. valueP |>> fun (name, (value, properties)) ->
          { name=name; value=value; properties=properties }
      sepBy nameValueP (pchar ',') |>> CorrelationContext

    let tryParse s =
      match run (headerValueP .>> eof) s with
      | Success (x, _, _) -> Result.Ok x
      | Failure (e, _, _) -> Result.Error e

    let tryParseOption s =
      match run (headerValueP .>> eof) s with
      | Success (x, _, _) -> Some x
      | Failure (_, _, _) -> None

  module TraceState =
    open FParsec

    let Header = "tracestate"

    let headerNameP: Parser<string, unit> =
      pstringCI Header <?> "'tracestate' string"

    let lcalphaP =
      let lcalphaPred =
        fun c ->
          int c >= 0x61 && int c <= 0x7A
          || int c >= int 'a' && int c <= int 'z'
      satisfy lcalphaPred

    let keyP: Parser<TraceStateKey, _> =
      let varP = manyChars2 lcalphaP (lcalphaP <|> digit <|> pchar '_' <|> pchar '-' <|> pchar '*' <|> pchar '/')
      let keyP = varP <?> "keyP"
      let vendorP = skipChar '@' >>. varP <?> "vendorSuffix alike '@custom_vendor'"
      (keyP .>>. opt vendorP) |>> TraceStateKey

    let valueP =
      let nblkChrP =
        let inRange i =
             i >= 0x21 && i <= 0x2B
          || i >= 0x2D && i <= 0x3C
          || i >= 0x3E && i <= 0x7E
        satisfy (fun c -> inRange (int c))
      let chrP = pchar (char 0x20) <|> nblkChrP
      manyChars2 chrP nblkChrP <?> "valueP"

    let listMemberP: Parser<_, _> =
      pipe3 keyP (skipChar '=') valueP (fun k _ v -> k, v)
      <?> "listMemberP"

    let listP =
      sepBy listMemberP (pchar ',')
      <?> "listP"

    let tryParse s =
      match run (listP .>> eof) s with
      | Success (x, _, _) -> Result.Ok x
      | Failure (e, _, _) -> Result.Error e

    let tryParseOption s =
      match run (listP .>> eof) s with
      | Success (x, _, _) -> Some x
      | Failure (_, _, _) -> None

  /// https://www.w3.org/TR/trace-context/#version-format
  let ExtractRegex = Regex("^
    (?<version>[0-9A-F]{2})-
    (?<traceId>[0-9A-F]{32})-
    (?<parentId>[0-9A-F]{16})-
    (?<traceFlags>[0-9A-F]{2})
    $", RegexOptions.IgnorePatternWhitespace ||| RegexOptions.IgnoreCase ||| RegexOptions.Compiled)

  let inline private parseByteHex value =
    match Byte.TryParse (value, NumberStyles.AllowHexSpecifier, Culture.invariant) with
    | false, _ -> None
    | true, v -> Some v

  let inline private parseInt32Hex value =
    match Int32.TryParse (value, NumberStyles.AllowHexSpecifier, Culture.invariant) with
    | false, _ -> None
    | true, v -> Some v


  open Option.Operators

  let extract (getter: Getter<'a>) (source: 'a): SpanAttr list * SpanContext option =

    let traceStateO =
      let headers = getManyExact getter source TraceState.Header
      headers
        |> List.concat
        |> List.map TraceState.tryParseOption
        |> List.filter Option.isSome
        |> List.map Option.get
        |> List.concat
        |> function [] -> None | tss -> TraceState.ofList tss |> Some

    let addTraceState (ctx: SpanContext) =
      match traceStateO with
      | None -> ctx
      | Some state -> ctx.withState state

    let parse version traceId parentSpanId flags =
          fun ver (flags: SpanFlags) ->
            let t, s, f = EdgeCaseLogic.applyWithFlags (ver, traceId, parentSpanId) flags
            SpanContext(t, s, f)
      <!> parseByteHex version
      <*> (parseInt32Hex flags |> Option.map enum<SpanFlags>)

    let spanCtx =
      getSingleExact getter source TraceParentHeader
        |> Option.bind (function
          | Regex ExtractRegex [ version; traceId; parentSpanId; flags ] ->
            let traceId, parentSpanId = TraceId.ofString traceId, SpanId.ofString parentSpanId
            parse version traceId parentSpanId flags
          | _ ->
            None)
        |> Option.map addTraceState

    let attrs: SpanAttr list =
      []

    let traceContext: Map<string, string> =
      getSingleExact getter source CorrelationContext.Header
        |> Option.bind CorrelationContext.tryParseOption
        |> Option.map (fun ctx -> ctx.valueStrings) // TO CONSIDER: forwarding properties!
        |> Option.defaultValue Map.empty

    attrs,
    if Map.isEmpty traceContext then spanCtx
    else spanCtx |> Option.map (fun ctx -> ctx.withContext traceContext)

  let inject (setter: Setter<'a>) (ctx: SpanContext) (target: 'a): 'a =
    let value = sprintf "00-%s-%O-%02x" (ctx.traceId.To32HexString()) ctx.spanId (int ctx.flags)
    setter (TraceParentHeader, value :: []) target

  let propagator =
    { new Propagator with
        member x.extract(getter, carrier) =
          extract getter carrier
        member x.inject(setter, ctx, carrier) =
          inject setter ctx carrier
       }

module internal JaegerBaggage =
  let Prefix = "uberctx-"

  let extract (source: (string * string list) list): Map<string, string> =
    [ for header, values in source do
        match values with
        | valueEncoded :: _ ->
          let key = header.Substring(Prefix.Length)
          yield key, Uri.UnescapeDataString valueEncoded
        | _ -> ()
    ]
    |> Map.ofSeq


  let inject setter (context: SpanContext) target =
    context.traceContext
      |> Seq.map (fun (KeyValue (k, v)) -> sprintf "%s%s" Prefix k, (SpanAttrValue.S v).uriEncode() :: [])
      |> Seq.fold (fun x data -> setter data x) target


/// https://www.jaegertracing.io/docs/1.14/client-libraries/#trace-span-identity
/// `uber-trace-id: {trace-id}:{span-id}:{parent-span-id}:{flags}`
///
/// where
///
/// `{trace-id}`:
///   -  64-bit or 128-bit random number in base16 format
///   -  Can be variable length, shorter values are 0-padded on the left
///   -  Clients in some languages support 128-bit, migration pending
///   -  Value of 0 is invalid
/// `{span-id}`:
///   -  64-bit random number in base16 format
/// `{parent-span-id}`:
///   -  64-bit value in base16 format representing parent span id
///   -  Deprecated, most Jaeger clients ignore on the receiving side, but still include it on the sending side
///      0 value is valid and means “root span” (when not ignored)
/// `{flags}`:
///   - One byte bitmap, as two hex digits
///   - Bit 1 (right-most, least significant) is “sampled” flag
///        * 1 means the trace is sampled and all downstream services are advised to respect that
///        * 0 means the trace is not sampled and all downstream services are advised to respect that
///            * We’re considering a new feature that allows downstream services to upsample if they find their tracing level is too low
///   - Bit 2 is “debug” flag
///        * Debug flag should only be set when the sampled flag is set
///        * Instructs the backend to try really hard not to drop this trace
///   - Other bits are unused
module Jaeger =
  open Extract

  /// https://github.com/jaegertracing/jaeger-client-node#debug-traces-forced-sampling
  ///
  /// When Jaeger sees this header in the request that otherwise has no tracing context, it ensures that the new trace started for this request will be sampled in the "debug" mode (meaning it should survive all downsampling that might happen in the collection pipeline), and the root span will have a tag as if this statement was executed:
  /// `span.setTag('jaeger-debug-id', 'some-correlation-id')`
  let DebugIdHeader = "jaeger-debug-id"
  let TraceHeader = "uber-trace-id"
  let BaggageHeaderPrefix = JaegerBaggage.Prefix


  let ExtractRegex = Regex("^([A-F0-9]{32}|[A-F0-9]{16}):([A-F0-9]{1,16}):([A-F0-9]{0,16}):(\d)$",
                           RegexOptions.IgnorePatternWhitespace ||| RegexOptions.IgnoreCase ||| RegexOptions.Compiled)


  let extract (get: Getter<'t>) (source: 't): SpanAttr list * SpanContext option =

    let context =
      getSingleExact get source TraceHeader
        |> Option.bind (function
          | Regex ExtractRegex [ traceId; spanId; parentSpanId; flags ] ->
            let traceId, spanId = TraceId.ofString traceId, SpanId.ofString spanId
            let psId =
              if parentSpanId = "" || parentSpanId = "0"
              then None else Some (SpanId.ofString parentSpanId)
            let flags = enum<SpanFlags>(int flags)
            Some (SpanContext(traceId, spanId, flags, ?parentSpanId=psId))
          | _ ->
            None)

    let attrs: SpanAttr list =
      match get source DebugIdHeader with
      | (_, debugIdValue :: _) :: _ ->
        [ DebugIdHeader, SpanAttrValue.S debugIdValue ]
      | _ ->
        []

    let traceContext: Map<string, string> =
      get source BaggageHeaderPrefix |> JaegerBaggage.extract

    attrs,
    if Map.isEmpty traceContext then context
    else context |> Option.map (fun ctx -> ctx.withContext traceContext)

  let inject (setter: Setter<'t>) (context: SpanContext) (target: 't): 't =
    let psId = match context.parentSpanId with None -> "0" | Some psId -> psId.ToString()
    let trace = sprintf "%O:%O:%O:%i" context.traceId context.spanId psId (byte context.flags)
    target
      |> setter (TraceHeader, trace :: [])
      |> JaegerBaggage.inject setter context

  let propagator =
    { new Propagator with
        member x.extract(getter, carrier) =
          extract getter carrier
        member x.inject(setter, ctx, carrier) =
          inject setter ctx carrier
    }


/// B3 trace format support
///
/// # Multiple headers [Docs](https://github.com/openzipkin/b3-propagation#multiple-headers)
///
///     X-B3-TraceId: 80f198ee56343ba864fe8b2a57d3eff7
///     X-B3-ParentSpanId: 05e3ac9a4f6e3b90
///     X-B3-SpanId: e457b5a2e4d86bd1
///     X-B3-Sampled: 1
///
/// OR (debug; `"X-B3-Sampled"` is omitted):
///
///     X-B3-TraceId: 80f198ee56343ba864fe8b2a57d3eff7
///     X-B3-ParentSpanId: 05e3ac9a4f6e3b90
///     X-B3-SpanId: e457b5a2e4d86bd1
///     X-B3-Flags: 1
///
/// Single header [Docs](https://github.com/openzipkin/b3-propagation#single-header),
/// [Rationale](https://github.com/openzipkin/b3-propagation/blob/master/RATIONALE.md):
///
///     b3: {traceId}-{spanId}-{samplingState}-{parentSpanId}
///     b3: {traceId}-{spanId}-{samplingState}
///     b3: 0
///
/// where
///
///     samplingState = '0' | '1' | 'd'
///
/// ### Examples:
///
///     b3: 80f198ee56343ba864fe8b2a57d3eff7-e457b5a2e4d86bd1-1-05e3ac9a4f6e3b90
///
/// OR (sampling decision = don't)
///
///     b3: 0
///
/// OR (no parentSpanId)
///
///     b3: 80f198ee56343ba864fe8b2a57d3eff7-e457b5a2e4d86bd1-1
///
/// OR (no parentSpanId, nor samplingState — root Span was NOT sampled)
///
///     b3:
///
/// ## Sharing SpanId
///
/// **We don't**, there's no `shared` flag in Logary:
/// [docs](https://github.com/openzipkin/brave/blob/master/brave/README.md#sharing-span-ids-between-client-and-server)
///
///  https://github.com/openzipkin/b3-propagation
module B3 =
  open Extract

  let BaggageHeaderPrefix = JaegerBaggage.Prefix
  let TraceHeader = "b3"
  let TraceIdHeader = "x-b3-traceid"
  let ParentSpanIdHeader = "x-b3-parentspanid"
  let SpanIdHeader = "x-b3-spanid"
  let FlagsHeader = "x-b3-flags"
  let SampledHeader = "x-b3-sampled"


  let ExtractRegex = Regex(@"^([A-F0-9]{32}|[A-F0-9]{16})-([A-F0-9]{1,16})(?:-(\d|d)(?:-([A-F0-9]{0,16}))?)?$",
                           RegexOptions.IgnorePatternWhitespace ||| RegexOptions.IgnoreCase ||| RegexOptions.Compiled)


  let internal parseFlags = function
    | "d" -> SpanFlags.Debug ||| SpanFlags.Sampled
    | "1" -> SpanFlags.Sampled
    | "0" | _ -> SpanFlags.None

  let extractMulti (get: Getter<'a>) (source: 'a): SpanAttr list * SpanContext option =
    let getFlags () =
      getSingleExact get source FlagsHeader
        |> Option.bind (function
          | "1" -> Some (SpanFlags.Debug ||| SpanFlags.Sampled)
          | _ -> None)
        |> Option.orElseWith (fun () ->
          getSingleExact get source SampledHeader
            |> Option.bind (function
              | "1" -> Some SpanFlags.Sampled
              | _ -> None)
        )
        |> Option.defaultValue SpanFlags.None

    [],
    getSingleExact get source TraceIdHeader |> Option.bind (fun traceId ->
    getSingleExact get source SpanIdHeader |> Option.map (fun spanId ->
    let flags = getFlags ()
    let parentSpanIdO = getSingleExact get source ParentSpanIdHeader |> Option.map SpanId.ofString
    let traceContext = get source BaggageHeaderPrefix |> JaegerBaggage.extract
    SpanContext(TraceId.ofString traceId, SpanId.ofString spanId, flags, ?parentSpanId=parentSpanIdO, ?traceContext=Some traceContext)))

  let injectMulti (setter: Setter<'a>) (context: SpanContext) (target: 'a): 'a =
    let flagsHeader, flagsValue =
      if context.isDebug then FlagsHeader, "1"
      elif context.isSampled then SampledHeader, "1"
      else SampledHeader, "0"

    target
      |> setter (flagsHeader, flagsValue :: [])
      |> setter (TraceIdHeader, context.traceId.ToString() :: [])
      |> setter (SpanIdHeader, context.spanId.ToString() :: [])
      |> match context.parentSpanId with
         | None -> id
         | Some psId -> setter (ParentSpanIdHeader, psId.ToString() :: [])
      |> JaegerBaggage.inject setter context


  let extractSingle (get: Getter<'a>) (source: 'a): SpanAttr list * SpanContext option =
    let context =
      getManyExact get source TraceHeader
        |> List.concat
        |> List.tryPick (function
          | Regex ExtractRegex [ traceId; spanId; flags; parentSpanId ] ->
            let psId = SpanId.ofString parentSpanId
            Some (traceId, spanId, parseFlags flags, Some psId)

          | Regex ExtractRegex [ traceId; spanId; flags ] ->
            Some (traceId, spanId, parseFlags flags, None)

          | Regex ExtractRegex [ traceId; spanId ] ->
            Some (traceId, spanId, SpanFlags.None, None)
          | _ ->
            None)
        |> Option.map (fun (traceId, spanId, flags, psIdO) ->
          SpanContext(TraceId.ofString traceId, SpanId.ofString spanId, flags, ?parentSpanId=psIdO)
        )

    let traceContext: Map<string, string> =
      get source BaggageHeaderPrefix |> JaegerBaggage.extract

    [],
    if Map.isEmpty traceContext then context
    else context |> Option.map (fun ctx -> ctx.withContext traceContext)


  let injectSingle (setter: Setter<'a>) (context: SpanContext) (target: 'a): 'a =
    let flags =
      if context.isDebug then "d"
      elif context.isSampled then "1"
      else "0"

    let parentSpanId =
      match context.parentSpanId with
      | None -> ""
      | Some psId -> sprintf "-%O" psId

    let trace = sprintf "%O-%O-%s%s" context.traceId context.spanId flags parentSpanId

    target
      |> setter (TraceHeader, trace :: [])
      |> JaegerBaggage.inject setter context


  let extract (get: Getter<'a>) (source: 'a): SpanAttr list * SpanContext option =
    let attrs, scO = extractSingle get source
    match scO with
    | None ->
      extractMulti get source
    | Some sc ->
      attrs, Some sc


  let inject (setter: Setter<'a>) (ctx: SpanContext) (target: 'a): 'a =
    injectSingle setter ctx target


  let propagator =
    { new Propagator with
        member x.extract(getter, carrier) =
          extract getter carrier
        member x.inject(setter, ctx, carrier) =
          inject setter ctx carrier
    }


module Combined =

  /// Chooses the first propagator that matches. It is the first propagator in the list that does the injection.
  let choose: Propagator list -> Propagator =
    fun ps ->
      if ps.Length = 0 then invalidArg "ps" "You must provide at least one propagator"
      { new Propagator with
          member x.extract<'t>(getter, instance: 't) =
            let rec tryExtract = function
              | [] ->
                [], None
              | p: Propagator :: tail ->
                match p.extract(getter, instance) with
                | sAs, Some ctxO ->
                  sAs, Some ctxO
                | _, _ ->
                  tryExtract tail
            tryExtract ps

          member x.inject(setter, ctx, target) =
            let p = List.head ps
            p.inject(setter, ctx, target)
      }
