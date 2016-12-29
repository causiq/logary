namespace Logary.Serialisation

open System
open Logary
open Logary.Serialisation.Chiron
open Logary.Serialisation.Chiron.Operators

module internal Json =

  let inline maybeWrite key value =
    match value with
    | Some v -> Json.write key v
    | None -> fun json -> Value (), json

[<AutoOpen>]
module internal Helpers =
  let (|Val|_|) = Map.tryFind

  let (|Only|_|) name m =
    match m with
    | Val name value when m |> Seq.length = 1 ->
      Some value

    | _ ->
      None

  let inline internal (|PropertyWith|) (fromJson : Json< ^a>) key =
       Lens.getPartial (Json.Object_ >??> key_ key)
    >> Option.bind (fromJson >> function | Value a, _ -> Some (a : 'a)
                                         | _, _ -> None)


module DataModel =

  let logLevel =
    let ser (level : LogLevel) : Json<unit> =
      Json.Optic.set Json.String_ (level.ToString())
    let deser : Json<LogLevel> =
          LogLevel.ofString
      <!> Json.Optic.get Json.String_
    ser, deser

  let value =
    let ser (v : Value) =
      match v with
      | String s ->
        Json.Optic.set Json.String_ s
      | Bool b ->
        Json.Optic.set Json.Bool_ b
      | Float f ->
        Json.Optic.set Json.Number_ (decimal f)
      | Int64 i ->
        Json.write "Int64" (decimal i)
      | BigInt bi ->
        Json.write "BigInt" (decimal bi)
      | Binary (bs, contentType) ->
        Json.write "mime" contentType
        *> Json.write "data" ("base64:" + Convert.ToBase64String bs)
      | Fraction (n, d) ->
        Json.write "fraction" (Json.Array [Json.Number (decimal n); Json.Number (decimal d)])
      | Array values ->
        Json.Optic.set Json.Array_ (values |> List.map Json.serialize)
      | Object o ->
        Json.Optic.set Json.Object_ (o |> Map.map (fun k v -> Json.serialize v))

    let deser : Json<Value> =
      fun json ->
        match json with
        | Json.String str ->
          JsonResult.Value (String str), json

        | Json.Bool b ->
          JsonResult.Value (Bool b), json

        | Json.Number f ->
          JsonResult.Value (Float (float f)), json

        | Json.Array arr ->
          match arr |> List.traverseChoiceA Json.tryDeserialize with
          | Choice1Of2 values ->
            JsonResult.Value (Array values), json
          | Choice2Of2 err ->
            JsonResult.Error err, json

        | Json.Object o ->

          match o with
          | Only "Int64" (Json.Number i) -> JsonResult.Value (Int64 (int64 i)), json
          | Only "BigInt" (Json.Number i) -> JsonResult.Value (BigInt (bigint i)), json
          | Val "mime" (Json.String mimeType) & Val "data" (Json.String data)
              when data.StartsWith "base64:" ->
            let bytes = Convert.FromBase64String (data.Substring(7))
            JsonResult.Value (Binary (bytes, mimeType)), json
          | Only "fraction" (Json.Array [Json.Number n; Json.Number d]) ->
            JsonResult.Value (Fraction (int64 n, int64 d)), json
          | _ ->
            (o |> Map.map (fun k v -> Json.deserialize v) |> Object) |> JsonResult.Value, json

        | Json.Null () ->
          JsonResult.Error "Cannot handle Null json values", json

    ser, deser

  let pointName =
    let ser (PointName segments) : Json<unit> =
      let data =
        segments
        |> Array.map String
        |> List.ofArray

      Json.Optic.set Json.Array_ data

    let deser : Json<PointName> =
      fun json ->
        Json.tryDeserialize json
        |> function
        | Choice1Of2 xs -> Json.init (PointName xs) json
        | Choice2Of2 err -> Json.error err json

    ser, deser

  let units =
    let ser (u : Units) : Json<unit> =
      match u with
      | Bits
      | Bytes
      | Seconds
      | Metres
      | Scalar
      | Amperes
      | Kelvins
      | Moles
      | Candelas
      | Percent
      | Watts
      | Hertz ->
        Json.Lens.setPartial Json.String_ (u |> Units.symbol)
      | Other other ->
        Json.write "other" other
      | Scaled (units, scale) ->
        Json.write "units" units
        *> Json.write "scale" scale
      | Offset (units, offset) ->
        Json.write "units" units
        *> Json.write "offset" offset
      | Mul (a, b) ->
        Json.write "multipleA" a
        *> Json.write "multipleB" b
      | Pow (a, b) ->
        Json.write "base" a
        *> Json.write "exponent" b
      | Div (a, b) ->
        Json.write "dividend" a
        *> Json.write "divider" b
      | Root a ->
        Json.write "root" a
      | Log10 a ->
        Json.write "log10" a

    let deser : Json<Units> =
      fun json ->
        match json with
        | Json.String s ->
          match s with
          | "bit" -> Bits |> JsonResult.Value, json
          | "B" -> Bytes |> JsonResult.Value, json
          | "s" -> Seconds |> JsonResult.Value, json
          | "m" -> Metres |> JsonResult.Value, json
          | "" -> Scalar |> JsonResult.Value, json
          | "A" -> Amperes |> JsonResult.Value, json
          | "K" -> Kelvins |> JsonResult.Value, json
          | "mol" -> Moles |> JsonResult.Value, json
          | "cd" -> Candelas |> JsonResult.Value, json
          | "%" -> Percent |> JsonResult.Value, json
          | "W" -> Watts |> JsonResult.Value, json
          | "Hz" -> Hertz |> JsonResult.Value, json
          | unknown ->
            let msg = sprintf "Unknown unit type represented as string '%s'" unknown
            JsonResult.Error msg, json

        | Json.Object o ->
          match o with
          | Val "other" other ->
            Other (Json.deserialize other) |> JsonResult.Value, json
          | Val "scale" scale & Val "units" units ->
            Scaled (Json.deserialize units, Json.deserialize scale) |> JsonResult.Value, json
          | Val "offset" offset & Val "units" units ->
            Offset (Json.deserialize units, Json.deserialize offset) |> JsonResult.Value, json
          | Val "multipleA" a & Val "multipleB" b ->
            Mul(Json.deserialize a, Json.deserialize b) |> JsonResult.Value, json
          | Val "base" b & Val "exponent" e ->
            Pow(Json.deserialize b, Json.deserialize e) |> JsonResult.Value, json
          | Val "dividend" a & Val "divider" b ->
            Div(Json.deserialize a, Json.deserialize b) |> JsonResult.Value, json
          | Val "root" a ->
            Root (Json.deserialize a) |> JsonResult.Value, json
          | Val "log10" a ->
            Log10 (Json.deserialize a) |> JsonResult.Value, json
          | _ -> JsonResult.Error "Unknown unit type represented as object", json
        | _ -> JsonResult.Error "Unknown unit", json

    ser, deser

  let valueUnits =
    let ser (value : Value, units : Units) : Json<unit> =
      Json.write "value" value
      *> Json.write "units" units

    let deser : Json<Value * Units> =
      (fun value units -> value, units)
      <!> Json.read "value"
      <*> Json.read "units"

    ser, deser

  let pointValue =

    let ser (pv : PointValue) =
      let inJsonObject writer =
        writer (Json.Object Map.empty) |> snd

      match pv with
      | Gauge (value, units) ->
        Json.writeWith (PointValue.valueUnitsToJson >> inJsonObject) "gauge" (value, units)

      | Derived (value, units) ->
        Json.writeWith (PointValue.valueUnitsToJson >> inJsonObject) "derived" (value, units)

      | Event template ->
        Json.write "event" template

    let deser : Json<POintValue> =
      fun json ->
        match json with
        | Chiron.PropertyWith PointValue.valueUnitsFromJson "gauge" (Some (value, units)) ->
          JsonResult.Value (Gauge (value, units)), json

        | Chiron.PropertyWith PointValue.valueUnitsFromJson "derived" (Some (value, units)) ->
          JsonResult.Value (Derived (value, units)), json

        // TODO: as opposed to here...
        | Property "event" event ->
          JsonResult.Value (Event event), json

        | json ->
          Json.error (sprintf "Cannot convert JSON %A to PointValue" json) json

    ser, deser

  let field =
    let ser (Field (value, maybeUnit)) : Json<unit> =
      Json.write "value" value
      *> Json.maybeWrite "units" maybeUnit

    let deser : Json<Field> =
      // not all fields have units; be flexible in format
      // for those that don't to make it easier for javascript
      // users
      fun json ->
        match json with
        | Json.Object o ->
          match o with
          | Only "value" value ->
            Field (Json.deserialize value, None)
            |> JsonResult.Value, json
          | Val "value" value & Val "units" units ->
            Field (Json.deserialize value, Some (Json.deserialize units))
            |> JsonResult.Value, json
          | _ ->
            Field (Json.deserialize json, None)
            |> JsonResult.Value, json
        | _ ->
          Field (Json.deserialize json, None)
          |> JsonResult.Value, json

    ser, deser

  let message =
    let ser (m : Message) : Json<unit> =
      let fields' =
        m.fields
        |> Seq.map (function KeyValue(key, value) -> PointName.format key, value)
        |> Map.ofSeq

      Json.write "name" m.name
      *> Json.write "value" m.value
      *> Json.write "fields" fields'
      *> Json.write "context" m.context
      *> Json.write "level" m.level
      *> Json.write "timestamp" m.timestamp

    let deser : Json<Message> =
      (fun name value (fields : Map<string, _>) context level ts ->
        let fields =
          fields
          |> Seq.map (fun kv -> PointName (kv.Key |> String.splita '.'), kv.Value)
          |> Map.ofSeq

        { name      = name
          value     = value
          fields    = fields
          context   = context
          level     = level
          timestamp = ts })
      <!> Json.read "name"
      <*> Json.read "value"
      <*> Json.read "fields"
      <*> Json.read "context"
      <*> Json.read "level"
      <*> Json.read "timestamp"

    ser, deser