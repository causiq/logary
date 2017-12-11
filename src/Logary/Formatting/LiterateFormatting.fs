namespace Logary.Formatting

open System
open System.IO
open Logary
open Logary.MessageTemplates
open Logary.MessageTemplates.Destructure
open Logary.MessageTemplates.Formatting
open Logary.MessageTemplates.Formatting.Literate

module Literate =
  let tokeniseExceptions (pvd: IFormatProvider) (nl: string) (exns: exn list) =
    let windowsStackFrameLinePrefix = "   at "
    let monoStackFrameLinePrefix = "  at "
    exns
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

  let tokeniseTemplateByGauges (pvd: IFormatProvider) (gauges : List<string * Gauge>) =
    if gauges.Length = 0 then Seq.empty
    else
      seq {
        yield "Gauges: ", Text
        yield "[", Punctuation

        let lastIndex = gauges.Length - 1
        for i=0 to lastIndex do
          let (gaugeType, Gauge (value, units)) = gauges.[i]
          let (scaledValue, unitsFormat) = Units.scale units value

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

  let tokeniseTemplateByFields (pvd: IFormatProvider) (template : Template) (destr : Destructurer) (fields : seq<string * obj>) =
    let fieldsMap = fields |> HashMap.ofSeq
    let tryGetPropertyValue (pt:Logary.Internals.FsMtParserFull.Property) =
      match HashMap.tryFind pt.name fieldsMap with
      | Some (value) ->
        destr {value = value; hint = (DestrHint.fromCaptureHint pt.captureHint); idManager = RefIdManager()} |> Some
      | _ -> None
    tokeniseTemplate pvd template tryGetPropertyValue

  let tokeniseTemplateWithGauges (pvd: IFormatProvider) destr message =
    let tplByGauges =
      message |> Message.getAllGauges |> List.ofSeq |> tokeniseTemplateByGauges pvd |> List.ofSeq

    let formatTemplate = message.value

    if String.IsNullOrEmpty formatTemplate then tplByGauges |> Seq.ofList
    else
      let parsedTemplate = parseToTemplate (formatTemplate)
      let tplByFields =
        message |> Message.getAllFields |> tokeniseTemplateByFields pvd parsedTemplate destr
      if List.isEmpty tplByGauges then tplByFields
      else seq { yield! tplByFields; yield " ", Subtext; yield! tplByGauges}

  let tokeniseContext (writeState: WriteState) (nl: string) (destr: Destructurer) (message: Message) =
    let padding = new String (' ', 4)
    let destrByStructure data = destr { value = data; idManager = writeState.idManager; hint= DestrHint.Structure }
    let destrByCaptureHint data captureHint = destr { value = data; idManager = writeState.idManager; hint= (DestrHint.fromCaptureHint captureHint) }
    let inline tokenisePropValues (writeState: WriteState) (prefix: string) (nl: string) (kvs: list<string * TemplatePropertyValue>) =
      let valuesToken =
        kvs
        |> Seq.collect (fun (name, tpv) -> seq {
           yield nl, Text
           yield padding, Text
           yield name, Subtext
           yield " => ", Punctuation
           yield! tokenisePropValueIndent writeState tpv nl 1
           })
        |> List.ofSeq

      if not <| List.isEmpty valuesToken then
        [
          yield nl, Text
          yield prefix, Text
          yield! valuesToken
        ]
      else List.empty

    // process fields
    let formatTemplate = message.value
    let fieldsPropInTemplate =
      if not <| String.IsNullOrEmpty formatTemplate then
        let parsedTemplate = parseToTemplate (formatTemplate)
        parsedTemplate.Properties |> Seq.map (fun prop -> prop.name, prop) |> HashMap.ofSeq
      else HashMap.empty

    let fields =
      message
      |> Message.getAllFields
      |> Seq.map (fun (name, value) ->
         // since we can have multi property with one field object, 
         // so maybe use Structure for all fields, or show fields depend on properties in template
         match HashMap.tryFind name fieldsPropInTemplate with
         | None -> (name, destrByStructure value)
         | Some prop -> (name, destrByCaptureHint value prop.captureHint))
      |> List.ofSeq

    // process gauge
    let gauges =
      message |> Message.getAllGauges
      |> Seq.map (fun (k, gauge) -> (k, destrByStructure gauge))
      |> List.ofSeq

    // process others
    let others =
      message |> Message.getContextsOtherThanGaugeAndFields
      |> Seq.map (fun (k, v) -> (k, destrByStructure v))
      |> List.ofSeq

    // need generate all property values first, in order to get idManager to calculate whether or not to show its ref id
    seq {
      yield! fields |> tokenisePropValues writeState "  fields:" nl
      yield! gauges |> tokenisePropValues writeState "  gauges:" nl
      yield! others |> tokenisePropValues writeState "  others:" nl
    }
    