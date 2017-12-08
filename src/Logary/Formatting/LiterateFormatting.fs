namespace Logary.Formatting

open System
open System.IO
open Logary
open Logary.Internals.FsMessageTemplates

module internal MessageParts =
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

  let tokeniseTemplateByFields (pvd: IFormatProvider) (template : Template)
                                        (destr : Destructurer) (maxDepth : int)
                                        (fields : seq<string * obj>) =
    let fieldsMap = fields |> HashMap.ofSeq
    let propertiesMap = template.Properties |> Seq.map (fun p -> (p.Name, p.Destr)) |> HashMap.ofSeq
    let getValueByName name =
      match (HashMap.tryFind name fieldsMap, HashMap.tryFind name propertiesMap) with
      | Some (value), Some (destrHint) ->
        destr (DestructureRequest(destr, value, maxDepth, 1, hint=destrHint))
      | _ -> TemplatePropertyValue.Empty
    FsMessageTemplates.tokeniseTemplate template pvd getValueByName

  let tokeniseTemplateWithGauges pvd nl destr maxDepth message =
    let tplByGauges =
      message |> Message.getAllGauges |> List.ofSeq |> tokeniseTemplateByGauges pvd |> List.ofSeq

    let (Event (formatTemplate)) = message.value

    if String.IsNullOrEmpty formatTemplate then tplByGauges |> Seq.ofList
    else
      let parsedTemplate = Parser.parse (formatTemplate)
      let tplByFields =
        message |> Message.getAllFields |> tokeniseTemplateByFields pvd parsedTemplate destr maxDepth
      if List.isEmpty tplByGauges then tplByFields
      else seq { yield! tplByFields; yield " ", Subtext; yield! tplByGauges}

  let tokeniseContext (pvd: IFormatProvider) (nl: string) (destr: Destructurer) maxDepth message =
    let padding = new String (' ', 4)

    let inline processKvs (pvd: IFormatProvider) (prefix: string) (nl: string) (kvs: seq<string * obj * DestrHint>) =
      let valuesToken =
        kvs
        |> Seq.collect (fun (name, value, destrHint) -> seq {
           yield nl, Text
           yield padding, Text
           yield name, Subtext
           yield " => ", Punctuation
           let destrValue = destr (DestructureRequest(destr, value, maxDepth, 1, hint= destrHint))
           yield! FsMessageTemplates.tokenisePropValueIndent pvd destrValue nl 1
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
        let parsedTemplate = Parser.parse (formatTemplate)
        parsedTemplate.Properties |> Seq.map (fun prop -> prop.Name, prop) |> HashMap.ofSeq
      else HashMap.empty

    let fields =
      message
      |> Message.getAllFields
      |> Seq.map (fun (name, value) ->
         match HashMap.tryFind name fieldsPropInTemplate with
         | None -> (name, value, DestrHint.Destructure)
         | Some prop -> (name, value, prop.Destr))
      |> processKvs pvd "  fields:" nl

    // process gauge
    let gauges =
      message |> Message.getAllGauges
      |> Seq.map (fun (k, gauge) -> (k, box gauge, DestrHint.Destructure))
      |> processKvs pvd "  gauges:" nl

    // process others
    let others =
      message |> Message.getContextsOtherThanGaugeAndFields
      |> Seq.map (fun (k, v) -> (k, v, DestrHint.Destructure))
      |> processKvs pvd "  others:" nl

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

