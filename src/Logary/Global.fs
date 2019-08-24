namespace Logary.Internals

open Logary

/// This module keeps track of the LoggingConfig reference.
module internal Global =
  open NodaTime

  type T =
    { getLogger: PointName -> Logger
      /// Gets a logger by name and applies the passed middleware to it. You can
      /// also use `Logger.apply` on existing loggers to create new ones.
      getLoggerWithMiddleware: PointName * Middleware -> Logger
      getTimestamp: unit -> EpochNanoSeconds
      /// Gets the console semaphore. When the process is running with an attached
      /// tty, this function is useful for getting the semaphore to synchronise
      /// around. You must take this if you e.g. make a change to the colourisation
      /// of the console output.
      getConsoleSemaphore: unit -> obj }

  /// Null object pattern; will only return loggers that don't log.
  let defaultConfig =
    let c = SystemClock.Instance
    let s = obj ()
    let nl = NullLogger() :> Logger
    { getLogger = fun _ -> nl
      getLoggerWithMiddleware = fun (_, _) -> nl
      getTimestamp = fun () -> c.GetCurrentInstant().ToUnixTimeTicks() * Constants.NanosPerTick
      getConsoleSemaphore = fun () -> s }

  /// This is the "Global Variable" containing the last configured Logary
  /// instance. If you configure more than one logary instance this will be
  /// replaced.
  let internal configD = DVar.create defaultConfig

  /// The flyweight references the current configuration. If you want
  /// multiple per-process logging setups, then don't use the static methods,
  /// but instead pass a Logger instance around, setting the name field of the
  /// Message value you pass into the logger.
  type Flyweight(name: PointName) =
    let loggerD = configD |> DVar.map (fun cfg -> cfg.getLogger name)

    let ensureName (m: Message) =
      if m.name.isEmpty then { m with name = name } else m

    interface Logger with // flyweight
      member x.name = name

      member x.level =
        let logger = DVar.get loggerD in logger.level

      member x.logWithAck (putBufferTimeOut, level) msgFactory =
        let logger = DVar.get loggerD in logger.logWithAck (putBufferTimeOut, level) (msgFactory >> ensureName)

  /// Call to initialise Logary with a new Logary instance.
  let initialise cfg = DVar.set configD cfg

  let getStaticLogger (name: PointName) = Flyweight(name) :> Logger

  /// Gets the current timestamp.
  let getTimestamp () = let config = DVar.get configD in config.getTimestamp ()

  /// Returns the synchronisation object to use when printing to the console.
  let getConsoleSemaphore () = let config = DVar.get configD in config.getConsoleSemaphore ()

  /// Run the passed function under the console semaphore lock.
  let lockSem fn =
    lock (getConsoleSemaphore ()) fn

  module Json =

    open System.Collections.Concurrent
    open Logary.Internals.TypeShape.Core
    open Logary.Internals.Chiron
    open Logary.Formatting

    module E = Chiron.Serialization.Json.Encode
    module EI = Chiron.Inference.Json.Encode

    let private customJsonEncoderDic = new ConcurrentDictionary<TypeShape, JsonEncoderFactory>()

    let configureEncoder<'t> (factory: JsonEncoderFactory<'t>) =
      let untypedEncoder (resolver: JsonEncoder<obj>) (data: obj) =
        match data with
        | :? 't as typedData ->
          factory resolver typedData
        | _ ->
          let message = sprintf "Could not down-cast value to '%s'. Value is: %O" typeof<'t>.FullName data
          Json.String message
      customJsonEncoderDic.[shapeof<'t>] <- untypedEncoder

    let customJsonEncoderRegistry = lazy (
      // Example:
      //configureEncoder<Gauge>(fun resolve (Gauge (v, u)) ->
      //  let (vs, us) = Units.scale u (v.toFloat())
      //  E.string (sprintf "%s %s" (vs.ToString()) us))

      { new JsonEncoderRegistry with
          member __.tryGet (shape: TypeShape) =
            match customJsonEncoderDic.TryGetValue shape with
            | true, factory ->
              Some factory
            | false , _ ->
              customJsonEncoderDic.Keys
              |> Seq.tryFind (fun baseType -> baseType.Type.IsAssignableFrom shape.Type)
              |> Option.bind (fun key ->
                 match customJsonEncoderDic.TryGetValue key with
                 | true, factory -> Some factory
                 | false , _ -> None)
      })

  module Destructure =

    open System
    open System.Collections.Concurrent
    open Logary.MessageTemplates
    open Logary.MessageTemplates.Destructure

    let private customProjectionDic = new ConcurrentDictionary<Type, Projection.How>()

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

    let private customDestructureDic = new ConcurrentDictionary<Type,CustomDestructureFactory>()

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
        let (scaledValue, unitsFormat) = Units.scale units (value.toFloat())
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
            yield { Name = "Message"; Value = ScalarValue ex.Message }
            if not <| isNull ex.Data && ex.Data.Count > 0 then
              yield { Name = "Data"; Value = req.WithNewValue(ex.Data) |> resolver }
            if not <| isNull ex.StackTrace then
              yield { Name = "StackTrace"; Value = ScalarValue (string ex.StackTrace) }
            if not <| isNull ex.TargetSite then
              yield { Name = "TargetSite"; Value = req.WithNewValue(ex.TargetSite) |> resolver }
            if not <| isNull ex.Source then
              yield { Name = "Source"; Value = ScalarValue (ex.Source) }
            if not <| isNull ex.HelpLink then
              yield { Name = "HelpLink"; Value = ScalarValue (ex.HelpLink) }
            if ex.HResult <> 0 then
              yield { Name = "HResult"; Value = ScalarValue ex.HResult }
            if not <| isNull ex.InnerException then
              yield { Name = "InnerException"; Value = req.WithNewValue(ex.InnerException) |> resolver }
          ]

          StructureValue (refId, typeTag, nvs))

      configDestructure<Duration>(fun _ req -> ScalarValue req.Value)
      configDestructure<Trace.SpanId>(fun _ req -> ScalarValue (req.Value.ToString()))
      configDestructure<Trace.TraceId>(fun _ req -> ScalarValue (req.Value.ToString()))

      configDestructure<Trace.SpanData>(fun resolver req ->
        let data = req.Value
        let refCount = req.IdManager
        match refCount.TryShowAsRefId req with
        | _, Some pv -> pv
        | refId, None ->
          let typeTag = typeof<Trace.SpanData>.Name
          let nvs = [
            yield { Name = "TraceId"; Value = req.WithNewValue data.context.traceId |> resolver }
            if Option.isSome data.context.parentSpanId then
              yield { Name = "ParentSpanId"; Value = req.WithNewValue data.context.parentSpanId |> resolver }
            yield { Name = "SpanId"; Value = req.WithNewValue data.context.spanId |> resolver }
            yield { Name = "BeginAt"; Value = ScalarValue (Instant.ofEpoch data.started) }
            yield { Name = "EndAt"; Value = ScalarValue (Instant.ofEpoch data.finished) }
            yield { Name = "Duration"; Value = ScalarValue data.elapsed }
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

  let jsonEncoderRegistry = Json.customJsonEncoderRegistry.Value
  let destructureRegistry = Destructure.customDestructureRegistry.Value
  let projectionStrategy = Destructure.tryGetCustomProjection