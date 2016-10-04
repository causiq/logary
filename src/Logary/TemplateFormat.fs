[<AutoOpen>]
module Logary.TemplateFormat

open System
open Logary
open Utils.FsMessageTemplates

module internal LogaryCapturing =
  open Logary.Utils.FsMessageTemplates

  let rec convertTemplatePropertyToField (tpv : TemplatePropertyValue) : Value =
    match tpv with
    | ScalarValue v ->
      // TODO: consider types like Guid, DateTime, DateTimeOffset. Are they prematurely stringified in Value.ofObject?
      // Does that prevent us from using the message template format string later in the pipeline?
      Value.ofObject v

    | DictionaryValue kvpList ->
      let stringObjMap =
        kvpList
        |> List.map (fun (k, v) ->
          let keyValue = match k with | ScalarValue v -> v.ToString() | _ -> failwith "only scalar value keys are supported"
          keyValue, (convertTemplatePropertyToField v))
        |> Map.ofList
      Value.Object stringObjMap

    | SequenceValue tpvList ->
      Value.Array (tpvList |> List.map (convertTemplatePropertyToField))

    | StructureValue (typeTag, pnvList) ->
      Value.Object (
        pnvList
        |> List.map (fun pnv -> pnv.Name, convertTemplatePropertyToField pnv.Value)
        |> List.append [ "_typeTag", Value.String typeTag ]
        |> Map.ofList)

  let rec convertToNameAndField (pnv : PropertyNameAndValue) : string * Field =
    pnv.Name, Field ((convertTemplatePropertyToField pnv.Value), None)

  let capture (template : Utils.FsMessageTemplates.Template) ([<ParamArray>] args : obj[]) =
    Capturing.captureProperties template args
    |> Seq.map (convertToNameAndField)
    |> List.ofSeq

  let logaryDefaultDestructurer : Destructurer =
    Capturing.createCustomDestructurer (None) (Some Capturing.destructureFSharpTypes)

  let captureField (property : Utils.FsMessageTemplates.Property) value =
    logaryDefaultDestructurer (DestructureRequest (logaryDefaultDestructurer, value, 10, 0, property.Destr))
    |> convertTemplatePropertyToField
    |> fun f -> Field (f, None)

type Message with

  static member templateEvent<'T> (level : LogLevel, format : string) : ('T -> Message) =
    let template = Parser.parse format
    if isNull template.Named || template.Named.Length <> 1 then
      failwithf "Template '%s' must have exactly 1 named property" format
    let field = template.Named.[0]
    fun (v : 'T) ->
      Message.event level format
      |> Message.setFieldValue field.Name (LogaryCapturing.captureField field v)

  static member templateEvent<'T1, 'T2> (level : LogLevel, format : string) : ('T1 -> 'T2 -> Message) =
    let template = Parser.parse format
    if isNull template.Named || template.Named.Length <> 2 then
      failwithf "Template '%s' must have exactly 2 named properties" format
    let field1 = template.Named.[0]
    let field2 = template.Named.[1]
    fun (v1 : 'T1) (v2 : 'T2) ->
      Message.event level format
      |> Message.setFieldValue field1.Name (LogaryCapturing.captureField field1 v1)
      |> Message.setFieldValue field2.Name (LogaryCapturing.captureField field2 v2)

  static member templateEvent<'T1, 'T2, 'T3> (level : LogLevel, format : string) : ('T1 -> 'T2 -> 'T3 -> Message) =
    let template = Parser.parse format
    if isNull template.Named || template.Named.Length <> 3 then
      failwithf "Template '%s' must have exactly 3 named properties" format
    let field1 = template.Named.[0]
    let field2 = template.Named.[1]
    let field3 = template.Named.[2]
    fun (v1 : 'T1) (v2 : 'T2) (v3 : 'T3) ->
      Message.event level format
      |> Message.setFieldValue field1.Name (LogaryCapturing.captureField field1 v1)
      |> Message.setFieldValue field2.Name (LogaryCapturing.captureField field2 v2)
      |> Message.setFieldValue field3.Name (LogaryCapturing.captureField field3 v3)

  static member templateEvent<'T1, 'T2, 'T3, 'T4> (level : LogLevel, format : string) : ('T1 -> 'T2 -> 'T3 -> 'T4 -> Message) =
    let template = Parser.parse format
    if isNull template.Named || template.Named.Length <> 4 then
      failwithf "Template '%s' must have exactly 4 named properties" format
    let field1 = template.Named.[0]
    let field2 = template.Named.[1]
    let field3 = template.Named.[2]
    let field4 = template.Named.[3]
    fun (v1 : 'T1) (v2 : 'T2) (v3 : 'T3) (v4 : 'T4) ->
      Message.event level format
      |> Message.setFieldValue field1.Name (LogaryCapturing.captureField field1 v1)
      |> Message.setFieldValue field2.Name (LogaryCapturing.captureField field2 v2)
      |> Message.setFieldValue field3.Name (LogaryCapturing.captureField field3 v3)
      |> Message.setFieldValue field4.Name (LogaryCapturing.captureField field4 v4)
