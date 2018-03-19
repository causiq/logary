namespace Logary.Formatting

open System
open System.IO
open Logary
open Logary.MessageTemplates
open Logary.MessageTemplates.Destructure
open Logary.MessageTemplates.Formatting
open Logary.MessageTemplates.Formatting.Literate

module Literate =
  let tokeniseExceptions (pvd: IFormatProvider) (nl: string) (m: Message) =
    let windowsStackFrameLinePrefix = "   at "
    let monoStackFrameLinePrefix = "  at "
    m
    |> Message.getErrors
    |> Seq.collect (fun exn ->
       let exnLines = new StringReader(string exn)
       seq {
         let mutable line = exnLines.ReadLine()
         while not (isNull line) do
           if line.StartsWith(windowsStackFrameLinePrefix) || line.StartsWith(monoStackFrameLinePrefix) then
             // subtext
             yield nl, Subtext
             yield line, Subtext
           else
             // regular text
             yield nl, Text
             yield line, Text
           line <- exnLines.ReadLine()
        })

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
          let scaledValue, unitsFormat = Units.scale units value

          match units with
          | Scaled (Seconds, scale) when scale = float Constants.NanosPerSecond ->
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
        message
        |> Message.getAllFields
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
