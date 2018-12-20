namespace Logary.Formatting

open System
open System.IO
open Logary
open Logary.MessageTemplates
open Logary.MessageTemplates.Destructure
open Logary.MessageTemplates.Formatting
open Logary.MessageTemplates.Formatting.Literate

module Literate =
  let rec private printLine = function
    | StacktraceLine.ExnType (et, m) ->
      [ String.Concat [ et; ": "; m ], Text ]
    | StacktraceLine.LineOutput data ->
      [ data, Subtext ]
    | StacktraceLine.Line line ->
      [ yield "  at ", Punctuation
        yield line.site, Subtext

        if Option.isSome line.file then
          yield " in ", Punctuation
          yield Option.get line.file, Subtext

        if Option.isSome line.lineNo then
          yield ":", Punctuation
          yield "line ", Subtext
          yield Option.get line.lineNo |> string, NumericSymbol
      ]
    | StacktraceLine.StacktraceDelim ->
      [ "--- End of exception stack trace ---", Punctuation ]

  /// Iterates through an exception hierarchy and uses `DotNetStacktrace.parse` on the stacktraces, yielding a
  /// the lines (outer list), each consisting of a string and a literate token specifying how that string is to be
  /// formatted.
  let rec private tokeniseException (e: exn): seq<(string * LiterateToken) list> =
    if isNull e then Seq.empty
    else
      // each inner list represents a written/outputted text line
      seq {
        yield printLine (ExnType (e.GetType().FullName, e.Message))
        yield! DotNetStacktrace.parse e.StackTrace |> Seq.map printLine
        match e with
        | :? AggregateException as ae when not (isNull ae.InnerExceptions) && ae.InnerExceptions.Count > 1 ->
          let mutable i = 0
          for e in ae.InnerExceptions do
            i <- i + 1
            let firstLine, remainder = tokeniseException e |> Seq.headTail
            yield printLine StacktraceDelim
            yield [
              yield sprintf "Inner exn#", Subtext
              yield string i, NumericSymbol
              yield sprintf " ", Punctuation
              yield! firstLine
            ]
            yield! remainder

        | _ when not (isNull e.InnerException) ->
          yield printLine StacktraceDelim
          yield! tokeniseException e.InnerException
        | _ ->
          ()
      }

  let tokeniseExceptions (pvd: IFormatProvider) (nl: string) (m: Message) =
    let exceptions =
      Message.getExns m |> Seq.collect tokeniseException

    let error =
      Message.tryGetError m
      |> Option.fold (fun s t -> t :: s) []
      |> Seq.collect id
      |> Seq.map printLine

    Seq.concat [ exceptions; error ]
    |> Seq.collect (fun line -> [ yield nl, Subtext; yield! line ])

  let tokeniseTemplateByGauges (pvd: IFormatProvider) (gauges: seq<string * Gauge>) =
    let gauges =
      Array.ofSeq gauges
      |> Array.sortBy fst
    if Array.isEmpty gauges then
      Seq.empty
    else
      seq {
        yield "Gauges: ", Text
        yield "[", Punctuation

        let lastIndex = gauges.Length - 1
        for i = 0 to lastIndex do
          let gaugeType, Gauge (value, units) = gauges.[i]
          let scaledValue, unitsFormat = Units.scale units (value.toFloat())

          match units with
          | Scaled (Seconds, scale) when scale = float Constants.NanosPerSecond ->
            let value = value.toFloat ()
            let format = if value < 1000. then "N0" else "N2"
            yield gaugeType, NameSymbol
            yield " took ", Subtext
            yield scaledValue.ToString(format, pvd), NumericSymbol
            if not <| String.IsNullOrEmpty unitsFormat then
              yield " ", Subtext
              yield unitsFormat, Text
            yield " to execute", Subtext
          | _ ->
            let valueFormated = Utils.formatWithCustom pvd scaledValue null
            yield gaugeType, Subtext
            yield ":", Punctuation
            yield " ", Subtext
            yield valueFormated, NumericSymbol
            if not <| String.IsNullOrEmpty unitsFormat then
              yield " ", Subtext
              yield unitsFormat, Text
          if i <> lastIndex then yield ", ", Punctuation

        yield "]", Punctuation
      }

  let tokeniseTemplateByFields (pvd: IFormatProvider) (template: Template) (resolver: DestructureResolver) (fields: seq<string * obj>) =
    let fieldsMap = HashMap.ofSeq fields
    let tryGetPropertyValue (pt: Logary.Internals.FsMtParserFull.Property) =
      match HashMap.tryFind pt.name fieldsMap with
      | Some value ->
        DestructureRequest(value, pt.captureHint.ToDestrHint(), RefIdManager ())
        |> resolver
        |> Some
      | _ ->
        None
    tokeniseTemplate pvd template tryGetPropertyValue

  let private plainContextValues (values: seq<string * obj>) =
    values
    |> Seq.filter (fun (k, v) -> not (k.StartsWith "_"))

  let tokeniseTemplateWithGauges (pvd: IFormatProvider) destr message =
    let tplByGauges =
      message
      |> Message.getAllGauges
      |> tokeniseTemplateByGauges pvd

    if String.IsNullOrEmpty message.value then
      tplByGauges
    else
      let parsedTemplate = parse message.value
      let tplByFields =
        Seq.concat [
          Message.getAllFields message
          Message.getOthers message
        ]
        |> tokeniseTemplateByFields pvd parsedTemplate destr
      if Seq.isEmpty tplByGauges then
        tplByFields
      else
        seq { yield! tplByFields; yield " ", Subtext; yield! tplByGauges }

  let tokeniseContext (writeState: WriteState) (nl: string) (resolver: DestructureResolver) (message: Message) =
    let padding = new String (' ', 4)

    let destrByStructure data =
      DestructureRequest(data, DestrHint.Structure, writeState.idManager)
      |> resolver

    let destrByCaptureHint data (captureHint: Logary.Internals.FsMtParserFull.CaptureHint) =
      DestructureRequest(data, captureHint.ToDestrHint(), writeState.idManager)
      |> resolver

    let inline tokenisePropValues (writeState: WriteState) (prefix: string) (nl: string) (kvs: (string * TemplatePropertyValue)[]) =
      let valuesToken =
        kvs
        |> Array.sortBy fst
        |> Array.collect (fun (name, tpv) ->
          [| yield nl, Text
             yield padding, Text
             yield name, Subtext
             yield " => ", Punctuation
             yield! tokenisePropValueIndent writeState tpv nl 1
          |])

      match valuesToken with
      | [||] ->
        Array.empty
      | _ ->
        [| yield nl, Text
           yield prefix, Text
           yield! valuesToken
        |]

    // process fields
    let fieldsPropInTemplate =
      if not (String.IsNullOrEmpty message.value) then
        let parsedTemplate = parse message.value
        parsedTemplate.Properties
        |> Seq.map (fun prop -> prop.name, prop)
        |> HashMap.ofSeq
      else
        HashMap.empty

    let fields =
      message
      |> Message.getAllFields
      |> Array.ofSeq
      |> Array.map (fun (name, value) ->
        // since we can have multi property with one field object,
        // so maybe use Structure for all fields, or show fields depend on properties in template
        match HashMap.tryFind name fieldsPropInTemplate with
        | None ->
          name, destrByStructure value
        | Some prop ->
          name, destrByCaptureHint value prop.captureHint)

    // process gauge
    let gauges =
      message
      |> Message.getAllGauges
      |> Array.ofSeq
      |> Array.map (fun (k, gauge) -> k, destrByStructure gauge)

    // process others
    let others =
      message
      |> Message.getOthers
      |> Array.ofSeq
      |> Array.map (fun (k, v) -> k, destrByStructure v)

    // need generate all property values first, in order to get idManager to calculate whether or not to show its ref id
    seq {
      yield! fields |> tokenisePropValues writeState "  fields:" nl
      yield! gauges |> tokenisePropValues writeState "  gauges:" nl
      yield! others |> tokenisePropValues writeState "  others:" nl
    }
