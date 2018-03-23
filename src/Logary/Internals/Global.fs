namespace Logary.Internals

open Logary

/// This module keeps track of the LoggingConfig reference.
module internal Global =
  open NodaTime

  type T =
    { getLogger: PointName -> Logger
      /// Gets a logger by name and applies the passed middleware to it. You can
      /// also use `Logger.apply` on existing loggers to create new ones.
      getLoggerWithMiddleware: PointName -> Middleware -> Logger
      getTimestamp: unit -> EpochNanoSeconds
      /// Gets the console semaphore. When the process is running with an attached
      /// tty, this function is useful for getting the semaphore to synchronise
      /// around. You must take this if you e.g. make a change to the colourisation
      /// of the console output.
      getConsoleSemaphore: unit -> obj }
    with
      static member create getLogger getLoggerWM getTs getCS =
        { getLogger = getLogger
          getLoggerWithMiddleware = getLoggerWM
          getTimestamp = getTs
          getConsoleSemaphore = getCS }

  /// Null object pattern; will only return loggers that don't log.
  let defaultConfig =
    let c = SystemClock.Instance
    let s = obj ()
    let nl = NullLogger() :> Logger
    { getLogger = fun pn -> nl
      getLoggerWithMiddleware = fun pn mid -> nl
      getTimestamp = fun () -> c.GetCurrentInstant().ToUnixTimeTicks() * Constants.NanosPerTick
      getConsoleSemaphore = fun () -> s }

  /// This is the "Global Variable" containing the last configured Logary
  /// instance. If you configure more than one logary instance this will be
  /// replaced.
  let internal config =
    ref (defaultConfig, (* logical clock *) 1u)

  /// The flyweight references the current configuration. If you want
  /// multiple per-process logging setups, then don't use the static methods,
  /// but instead pass a Logger instance around, setting the name field of the
  /// Message value you pass into the logger.
  type Flyweight(name: PointName) =
    // The object's private fields are initialised to the current config's
    // logger.
    let updating = obj()
    let mutable fwClock: uint32 = snd !config
    let mutable logger: Logger = (fst !config).getLogger name

    /// A function that tries to run the action with the current logger, and
    /// which reconfigures if the configuration is updated.
    let withLogger action =
      if snd !config <> fwClock then // if we are outdated
        lock updating <| fun _ ->
          let cfg, cfgClock = !config // reread the config's clock after taking lock
          if cfgClock <> fwClock then // recheck after taking lock to avoid races
            logger <- cfg.getLogger name // get the current logger
            fwClock <- cfgClock // update instance's clock

      // finally execute the action with the logger
      action logger

    let ensureName (m: Message) =
      if m.name.isEmpty then { m with name = name } else m

    interface Logger with // flyweight
      member x.name = name

      member x.level =
        withLogger (fun logger -> logger.level)

      member x.logWithAck level msgFactory =
        withLogger (fun logger -> logger.logWithAck level (msgFactory >> ensureName))


  /// Call to initialise Logary with a new Logary instance.
  let initialise cfg =
    config := (cfg, snd !config + 1u)

  let getStaticLogger (name: PointName) =
    Flyweight(name)

  /// Gets the current timestamp.
  let getTimestamp (): EpochNanoSeconds =
    (fst !config).getTimestamp ()

  /// Returns the synchronisation object to use when printing to the console.
  let getConsoleSemaphore () =
    (fst !config).getConsoleSemaphore()

  /// Run the passed function under the console semaphore lock.
  let lockSem fn =
    lock (getConsoleSemaphore ()) fn

  module Json =

    open System
    open System.Collections.Concurrent
    open Logary.Internals.Chiron
    open Logary.Formatting.JsonHelper

    module E = Chiron.Serialization.Json.Encode
    module EI = Chiron.Inference.Json.Encode

    let private customJsonEncoderDic = new ConcurrentDictionary<Type,CustomJsonEncoderFactory>()

    let configJsonEncoder<'t> (factory: CustomJsonEncoderFactory<'t>) =
      let ty = typeof<'t>
      let untypedEncoder (resolver: JsonEncoder<obj>) (data: obj) =
        match data with
        | :? 't as typedData ->
          factory resolver typedData
        | _ ->
          let message = sprintf "Could not down-cast value to '%s'. Value is: %O" typeof<'t>.FullName data
          Json.String message
      customJsonEncoderDic.[ty] <- untypedEncoder

    let internal customJsonEncoderRegistry = lazy (
      configJsonEncoder<PointName>(fun _ name -> E.string (name.ToString()))

      configJsonEncoder<Gauge>(fun _ (Gauge (v, u)) ->
        let (vs, us) = Units.scale u v
        E.string (sprintf "%s %s" (vs.ToString()) us))

      configJsonEncoder<Message>(fun resolver msg ->
        JsonObject.empty
        |> EI.required "name" (string msg.name)
        |> EI.required "value" msg.value
        |> EI.required "level" (string msg.level)
        |> EI.required "timestamp" msg.timestamp
        |> E.required resolver "context" msg.context
        |> JsonObject.toJson)

      { new ICustomJsonEncoderRegistry with
          member __.TryGetRegistration (runtimeType: Type) =
            match customJsonEncoderDic.TryGetValue runtimeType with
            | true, factory ->
              Some factory
            | false , _ ->
              customJsonEncoderDic.Keys
              |> Seq.tryFind (fun baseType -> baseType.IsAssignableFrom runtimeType)
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
        let (scaledValue, unitsFormat) = Units.scale units value
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