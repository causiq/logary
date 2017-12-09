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
            let valueFormated = formatWithProvider pvd scaledValue null
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

  let tokeniseTemplateByFields (writeState: WriteState) (template : Template) (destr : Destructurer) (fields : seq<string * obj>) =
    let fieldsMap = fields |> HashMap.ofSeq
    let tryGetPropertyValue (pt:Logary.FsMtParserFull.Property) =
      match HashMap.tryFind pt.name fieldsMap with
      | Some (value) ->
        destr {value = value; hint = (DestrHint.fromCaptureHint pt.captureHint); idManager = writeState.idManager} |> Some
      | _ -> None
    tokeniseTemplate template writeState tryGetPropertyValue

  let tokeniseTemplateWithGauges (writeState: WriteState) destr message =
    let tplByGauges =
      message |> Message.getAllGauges |> List.ofSeq |> tokeniseTemplateByGauges writeState.provider |> List.ofSeq

    let (Event (formatTemplate)) = message.value

    if String.IsNullOrEmpty formatTemplate then tplByGauges |> Seq.ofList
    else
      let parsedTemplate = parseToTemplate (formatTemplate)
      let tplByFields =
        message |> Message.getAllFields |> tokeniseTemplateByFields writeState parsedTemplate destr
      if List.isEmpty tplByGauges then tplByFields
      else seq { yield! tplByFields; yield " ", Subtext; yield! tplByGauges}

  let tokeniseContext (writeState: WriteState) (nl: string) (destr: Destructurer) (message: Message) =
    let padding = new String (' ', 4)

    let inline processKvs (writeState: WriteState) (prefix: string) (nl: string) (kvs: seq<string * obj * DestrHint>) =
      let valuesToken =
        kvs
        |> Seq.collect (fun (name, value, destrHint) -> seq {
           yield nl, Text
           yield padding, Text
           yield name, Subtext
           yield " => ", Punctuation
           let tpv = destr { value = value; idManager = writeState.idManager ; hint= destrHint }
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
    let (Event (formatTemplate)) = message.value
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
         | None -> (name, value, DestrHint.Structure)
         | Some prop -> (name, value, DestrHint.fromCaptureHint prop.captureHint))
      |> processKvs writeState "  fields:" nl

    // process gauge
    let gauges =
      message |> Message.getAllGauges
      |> Seq.map (fun (k, gauge) -> (k, box gauge, DestrHint.Structure))
      |> processKvs writeState "  gauges:" nl

    // process others
    let others =
      message |> Message.getContextsOtherThanGaugeAndFields
      |> Seq.map (fun (k, v) -> (k, v, DestrHint.Structure))
      |> processKvs writeState "  others:" nl

    [fields; gauges; others] |> Seq.concat

  let rec formatValueLeafs (ns : string list) (value : Value) =
    let rns = lazy (PointName.ofList (List.rev ns))
    seq {
      match value with
      | String s ->
        yield rns.Value, s
      | Bool b ->
        yield rns.Value, b.ToString()
      | Float f ->
        yield rns.Value, f.ToString()
      | Int64 i ->
        yield rns.Value, i.ToString ()
      | BigInt b ->
        yield rns.Value, b.ToString ()
      | Binary (b, _) ->
        yield rns.Value, BitConverter.ToString b |> fun s -> s.Replace("-", "")
      | Fraction (n, d) ->
        yield rns.Value, sprintf "%d/%d" n d
      | Array list ->
        for item in list do
          yield! formatValueLeafs ns item
      | Object m ->
        for KeyValue (k, v) in m do
          yield! formatValueLeafs (k :: ns) v
    }

