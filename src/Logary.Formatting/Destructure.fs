module Logary.Formatting.Destructure

open System
open System.Collections.Concurrent
open Logary
open Logary.Formatting.MessageTemplates
open Logary.Formatting.MessageTemplates.Destructure

let private customProjectionDic = ConcurrentDictionary<Type, Projection.How>()

let configProjection projectionExpr =
  match Projection.byExpr projectionExpr with
  | Projection.Projection (t, how) ->
    customProjectionDic.AddOrUpdate(t,how,fun _ _ -> how) |> ignore
  | Projection.NotSupport -> ()

let internal tryGetCustomProjection: Projection.ProjectionStrategy  =
  fun (t: Type) ->
    match customProjectionDic.TryGetValue t with
    | true, projection -> Some projection
    | false , _ ->
      customProjectionDic.Keys
      |> Seq.tryFind (fun baseType -> baseType.IsAssignableFrom t)
      |> Option.bind (fun key ->
         match customProjectionDic.TryGetValue key with
         | true, projection ->Some projection
         | false , _ -> None)

let private customDestructureDic = ConcurrentDictionary<Type, CustomDestructureFactory>()

let configDestructure<'t> (factory: CustomDestructureFactory<'t>) =
  let ty = typeof<'t>
  let untypedFactory resolver (untypedReq: DestructureRequest) =
    match untypedReq.TryDownCast<'t> () with
    | Some typedReq -> factory resolver typedReq
    | _ -> ScalarValue (sprintf "failed down cast to %A , obj is : %s" typeof<'t> (string untypedReq.Value))
  customDestructureDic.[ty] <- untypedFactory

let internal customDestructureRegistry = lazy (
  configDestructure<Gauge>(fun _ req ->
    let (Gauge (value, units)) =  req.Value
    let (scaledValue, unitsFormat) = U.scale units value.asFloat
    if String.IsNullOrEmpty unitsFormat then ScalarValue scaledValue
    else ScalarValue (sprintf "%s %s" (string scaledValue) unitsFormat))

  configDestructure<Exception>(fun resolver req ->
    let ex = req.Value
    let refCount = req.IdManager
    match refCount.TryShowAsRefId req with
    | _, Some pv -> pv
    | refId, None ->
      let typeTag = ex.GetType().FullName
      let nvs = [
        yield { Name = "message"; Value = ScalarValue ex.Message }
        if not <| isNull ex.Data && ex.Data.Count > 0 then
          yield { Name = "data"; Value = req.WithNewValue(ex.Data) |> resolver }
        if not <| isNull ex.StackTrace then
          yield { Name = "stackTrace"; Value = ScalarValue (string ex.StackTrace) }
        if not <| isNull ex.TargetSite then
          yield { Name = "targetSite"; Value = req.WithNewValue(ex.TargetSite) |> resolver }
        if not <| isNull ex.Source then
          yield { Name = "source"; Value = ScalarValue (ex.Source) }
        if not <| isNull ex.HelpLink then
          yield { Name = "helpLink"; Value = ScalarValue (ex.HelpLink) }
        if ex.HResult <> 0 then
          yield { Name = "hResult"; Value = ScalarValue ex.HResult }
        if not <| isNull ex.InnerException then
          yield { Name = "innerException"; Value = req.WithNewValue(ex.InnerException) |> resolver }
      ]

      StructureValue (refId, typeTag, nvs))

  configDestructure<NodaTime.Duration>(fun _ req -> ScalarValue req.Value)
  configDestructure<Trace.SpanId>(fun _ req -> ScalarValue (req.Value.ToString()))
  configDestructure<Trace.TraceId>(fun _ req -> ScalarValue (req.Value.ToString()))

  configDestructure<SpanMessage>(fun resolver req ->
    let data = req.Value
    let refCount = req.IdManager
    match refCount.TryShowAsRefId req with
    | _, Some pv -> pv
    | refId, None ->
      let typeTag = typeof<SpanMessage>.Name
      let nvs = [
        yield { Name = "TraceId"; Value = req.WithNewValue data.context.traceId |> resolver }
        if Option.isSome data.context.parentSpanId then
          yield { Name = "ParentSpanId"; Value = req.WithNewValue data.context.parentSpanId |> resolver }
        yield { Name = "SpanId"; Value = req.WithNewValue data.context.spanId |> resolver }
        yield { Name = "BeginAt"; Value = ScalarValue (Instant.ofEpoch data.started) }
        yield { Name = "EndAt"; Value = ScalarValue (Instant.ofEpoch data.finished) }
      ]

      StructureValue (refId, typeTag, nvs))

  {
    new ICustomDestructureRegistry with
      member __.TryGetRegistration (runtimeType: Type) =
        match customDestructureDic.TryGetValue runtimeType with
        | true, factory -> factory |> Some
        | false , _ ->
          customDestructureDic.Keys
          |> Seq.tryFind (fun baseType -> baseType.IsAssignableFrom runtimeType)
          |> Option.bind (fun key ->
             match customDestructureDic.TryGetValue key with
             | true, factory -> factory |> Some
             | false , _ -> None)
  })
