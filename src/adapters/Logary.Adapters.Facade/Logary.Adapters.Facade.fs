namespace Logary.Adapters.Facade

open System
open System.Reflection
open System.Collections.Concurrent
open FSharp.Reflection
open Castle.DynamicProxy
open Hopac
open Hopac.Extensions
open Hopac.Infixes
open Logary
open Logary.Internals

module Reflection =

  /// Gets the method on the type
  let findMethod : Type * string -> MethodInfo =
    Cache.memoize (fun (typ, meth) -> typ.GetMethod meth)

  /// Gets the property on the type
  let findProperty : Type * string -> PropertyInfo =
    Cache.memoize (fun (typ, prop) -> typ.GetProperty prop)

  let findStaticProperty : Type * string -> PropertyInfo =
    Cache.memoize (fun (typ, prop) -> typ.GetProperty(prop, BindingFlags.Static ||| BindingFlags.Public))

  let findField : Type * string -> FieldInfo =
    Cache.memoize (fun (typ, field) -> typ.GetField field)

  /// Finds the module relative to the logger type
  let findModule =
    let findModule_ : Type * string -> Type =
      fun (loggerType, moduleName) ->
        let typ = sprintf "%s.%s, %s" loggerType.Namespace moduleName loggerType.Assembly.FullName
        Type.GetType typ
    Cache.memoize findModule_

  let readLiteral (loggerType : Type) =
    Cache.memoize (fun (propertyName : string, defaultValue : 'a) ->
      let literals = findModule (loggerType, "Literals")
      let field = literals.GetProperty(propertyName, BindingFlags.Static ||| BindingFlags.Public)
      if isNull field then defaultValue
      else field.GetValue(null, null) :?> 'a)

  let versionFrom : Type -> uint32 =
    Cache.memoize (fun loggerType -> readLiteral loggerType ("FacadeVersion", 1u))

  type ApiVersion =
    /// This API version did not have an explicit version field.
    | V1
    /// NS.Literals.FacadeVersion
    | V2

    override x.ToString() =
      match x with
      | V1 -> "V1"
      | V2 -> "V2"

    static member ofType (loggerType : Type) =
      match versionFrom loggerType with
      | 2u ->
        V2
      | _ ->
        V1
  let (|FSharp|CSharp|) (loggerType : Type) =
    let version = ApiVersion.ofType loggerType
    let globals = findModule (loggerType, "Global")
    match readLiteral loggerType ("FacadeLanguage", "F#") with
    | "F#" ->
      let configType = findModule (loggerType, "LoggingConfig")
      FSharp (version, globals, configType)
    | "C#" ->
      let configType = findModule (loggerType, "ILoggingConfig")
      CSharp (version, globals, configType)
    | language ->
      failwithf "Unknown language '%s'" language

  let rawPrint (invocation : IInvocation) =
    printfn "Invocation"
    printfn "=========="
    printfn "Args: "
    for arg in invocation.Arguments do
      let argType = arg.GetType()
      printfn " - %A\n   : %s" arg (argType.FullName)
      printfn "   :> %s" argType.BaseType.FullName
    printfn "Method.Name: %s" invocation.Method.Name
    printfn "----------"

module LoggerAdapterShared =

  let unitOfString (s : string) =
    match s with
    | "bit" -> Bits
    | "B" -> Bytes
    | "s" -> Seconds
    | "m" -> Metres
    | "" -> Scalar
    | "A" -> Amperes
    | "K" -> Kelvins
    | "mol" -> Moles
    | "cd" -> Candelas
    | "%" -> Percent
    | "W" -> Watts
    | "Hz" -> Hertz
    | other -> Other other

  /// temporary solution
  type OldPointValue =
  | Event of string
  | Gauge of float * Units

/// Utilities for creating a single 'MyLib.Logging.Logger' in the target type
/// space. The original logger adapter (also see the LoggerCSharpAdapter further
/// below)
module LoggerAdapter =
  open Reflection

  let private defaultName (fallback : string[]) = function
    | [||] ->
      fallback
    | otherwise ->
      otherwise

  /// Convert the object instance to a PointValue. Is used from the
  /// other code in this module.
  let toPointValue (o : obj) : LoggerAdapterShared.OldPointValue =
    let typ = o.GetType()
    let info, values = FSharpValue.GetUnionFields(o, typ)
    match info.Name, values with
    | "Event", [| template |] ->
      LoggerAdapterShared.OldPointValue.Event (template :?> string)

    | "Gauge", [| value; units |] ->
      LoggerAdapterShared.OldPointValue.Gauge (float (value :?> int64), Units.Scalar)

    | caseName, values ->
      let valuesStr = values |> Array.map string |> String.concat ", "
      failwithf "Unknown union case '%s (%s)' on '%s'" caseName valuesStr typ.FullName

  /// Convert the object instance to a LogLevel. Is used from the
  /// other code in this module.
  let toLogLevel (o : obj) : LogLevel =
    let typ = o.GetType()
    let par = typ, "toInt"
    let toInt = findMethod (typ, "toInt")
    LogLevel.ofInt (toInt.Invoke(o, null) :?> int)

  /// Convert the object instance to a Logary.DataModel.Message. Is used from the
  /// other code in this module.
  let toMsg fallbackName (o : obj) : Message =
    let typ = o.GetType()
    let readProperty name = (findProperty (typ, name)).GetValue o
    let oldPointValue = readProperty "value" |> toPointValue
    let event = 
      match oldPointValue with
      | LoggerAdapterShared.OldPointValue.Event tpl -> Event tpl
      | _ -> Event (String.Empty)
    let fields = readProperty "fields" :?> Map<string, obj>
    { name      = PointName (readProperty "name" :?> string [] |> defaultName fallbackName)
      value     = event
      context   = HashMap.empty
      timestamp = readProperty "timestamp" :?> EpochNanoSeconds
      level     = readProperty "level" |> toLogLevel }
    |> Message.setFieldsFromMap fields
    |> (fun msg -> 
        match oldPointValue with
        | LoggerAdapterShared.OldPointValue.Gauge (value, units) ->
          msg |> Message.setGauge (value,units)
        | _ -> msg)

  /// Convert the object instance to a message factory method. Is used from the
  /// other code in this module.
  let toMsgFactory fallbackName oLevel (o : obj) : LogLevel -> Message =
    let typ = o.GetType()
    let invokeMethod = findMethod (typ, "Invoke")
    fun level ->
      toMsg fallbackName (invokeMethod.Invoke(o, [| oLevel |]))

  let internal (|Log|LogWithAck|LogSimple|) ((invocation, defaultName) : IInvocation * string[]) : Choice<_, _, _> =
    match invocation.Method.Name with
    | "log" ->
      let oLevel = invocation.Arguments.[0]
      let level = toLogLevel oLevel
      let factory = toMsgFactory defaultName oLevel invocation.Arguments.[1]
      Log (level, factory)

    | "logWithAck" ->
      let oLevel = invocation.Arguments.[0]
      let level = toLogLevel oLevel
      let factory = toMsgFactory defaultName oLevel invocation.Arguments.[1]
      LogWithAck (level, factory)

    | "logSimple" ->
      let msg = toMsg defaultName invocation.Arguments.[0]
      LogSimple msg

    | meth ->
      failwithf "Method '%s' should not exist on Logary.Facade.Logger" meth

  /// This is the main adapter which logs from an arbitrary logary facade
  /// into logary. Provide the namespace you put the facade in and the assembly
  /// which it should be loaded from, and this adapter will use (memoized) reflection
  /// to properly bind to the facade.
  type private I(logger : Logger, version : ApiVersion) =
    let (PointName defaultName) = logger.name

    // Codomains of these three functions are equal to codomains of Facade's
    // functions:

    let logWithAck (level : LogLevel) (messageFactory : LogLevel -> Message) : Async<unit> =
      // a placeholder for the Promise ack from Logary proper
      let prom = IVar ()

      // kick off the logging no matter what
      start (Logger.logWithAck logger level messageFactory ^=> IVar.fill prom)

      // take the promise from within the IVar and make it an Async (which is
      // "hot" in that starting it will return "immediately" and be idempotent)
      (prom ^=> id) |> Job.toAsync

    let logV1 level messageFactory : unit =
      // start immediate because in the normal case we can put the Message
      // in the RingBuffer without any extra time taken
      logWithAck level messageFactory |> Async.StartImmediate

    let logV2 level messageFactory : Async<unit> =
      logger.log level messageFactory |> Alt.toAsync

    let logSimple (msg : Message) : unit =
      logger.logSimple msg

    interface IInterceptor with
      member x.Intercept invocation =
        match invocation, defaultName with
        | Log (level, messageFactory) when version = V1 ->
          let ret = logV1 level messageFactory
          invocation.ReturnValue <- ret

        | Log (level, messageFactory) ->
          let ret = logV2 level messageFactory
          invocation.ReturnValue <- ret

        | LogWithAck (level, messageFactory) ->
          invocation.ReturnValue <- logWithAck level messageFactory

        // this was in V1 of the API
        | LogSimple message ->
          invocation.ReturnValue <- logSimple message

  /// Create a target assembly's logger from the given type, which delegates to
  /// the passed Logary proper Logger.
  [<CompiledName("Create")>]
  let create (typ : Type) logger : obj =
    if typ = null then invalidArg "typ" "is null"
    let generator = new ProxyGenerator()
    let facade = I (logger, ApiVersion.ofType typ) :> IInterceptor
    generator.CreateInterfaceProxyWithoutTarget(typ, facade)

  /// Create a target assembly's logger from the given type-string, which
  /// delegates to the passed Logary proper logger.
  [<CompiledName("Create")>]
  let createString (typ : string) logger : obj =
    create (Type.GetType typ) logger

  /// Creates a target assembly's logger from the passed generic logger type
  /// which delegates to the passed Logary-proper's logger. This is the function
  /// you'll normally use if you use this module. Otherwise, please see
  /// LogaryFacadeAdapter.
  [<CompiledName("Create")>]
  let createGeneric<'logger when 'logger : not struct> logger : 'logger =
    create typeof<'logger> logger :?> 'logger

module LoggerCSharpAdapter =
  open System.Threading
  open System.Threading.Tasks
  open Reflection
  open Logary.CSharp

  let private defaultName (fallback : string[]) = function
    | [||] ->
      fallback
    | otherwise ->
      otherwise

  let internal toPointValue (o : obj) : LoggerAdapterShared.OldPointValue =
    let typ = o.GetType()
    //printfn "Converting object=%A to PointValue" o
    if typ.Name = "Event" then
      let field = findField (typ, "Template")
      LoggerAdapterShared.OldPointValue.Event (field.GetValue(o) :?> string)
    elif typ.Name = "Gauge" then
      let valueField = findField (typ, "Value")
      let valueValue = valueField.GetValue(o) :?> int64
      let unitField = findField (typ, "Unit")
      let unitValue = unitField.GetValue(o) :?> string
      LoggerAdapterShared.OldPointValue.Gauge (float valueValue, LoggerAdapterShared.unitOfString unitValue)
    else
      failwithf "Unknown point value type name '%s'" typ.Name

  let internal toLogLevel (o : obj) : LogLevel =
    LogLevel.ofInt (unbox (Convert.ChangeType (o, typeof<int>)))

  /// Convert the object instance to a Logary.DataModel.Message. Is used from the
  /// other code in this module.
  let internal toMessage fallbackName (o : obj) : Message =
    let typ = o.GetType()
    let readProperty name = (findProperty (typ, name)).GetValue o
    let oldPointValue = readProperty "Value" |> toPointValue
    let event = 
      match oldPointValue with
      | LoggerAdapterShared.OldPointValue.Event tpl -> Event tpl
      | _ -> Event (String.Empty)
    { name      = PointName (readProperty "Name" :?> string [] |> defaultName fallbackName)
      value     = event
      context   = HashMap.empty
      timestamp = readProperty "Timestamp" :?> EpochNanoSeconds
      level     = readProperty "Level" |> toLogLevel }
    |> fun msg ->
      let folder msg (KeyValue (key, value)) =
        msg |> Message.setField key value
      readProperty "Fields" :?> System.Collections.Generic.IDictionary<string, obj> |> Seq.fold folder msg
    |> (fun msg ->
        match oldPointValue with
        | LoggerAdapterShared.OldPointValue.Gauge (value, units) ->
          msg |> Message.setGauge (value,units)
        | _ -> msg)

  module internal LogMessage =
    let create (loggerType : Type) : string[] -> obj -> obj =
      let logLevelT = findModule (loggerType, "LogLevel")
      let pointValueT = findModule (loggerType, "PointValue")
      let emptyDic =
        System.Collections.ObjectModel.ReadOnlyDictionary<string, obj>(
          Map.empty
        )
      let ctorTs =
        [| typeof<string>; logLevelT; pointValueT; emptyDic.GetType(); typeof<int64> |]
      let logMessageT =
        findModule (loggerType, "LogMessage")
      //printfn "%A ====> c'tor(%A)" logMessageT ctorTs
      let emptyValueT = findModule (loggerType, "PointValue")
      let emptyValue =
        findStaticProperty (emptyValueT, "Empty")
        |> fun t -> t.GetValue(null, null)

      fun (name : string[]) (level : obj) ->
        let ts =  Logary.Internals.Global.getTimestamp ()
        let args = [| box name; level; emptyValue; box emptyDic; box ts |]
        //printfn "====> %s(%A)" logMessageT.Name args
        Activator.CreateInstance(logMessageT, args)


  type private I (loggerType : Type, logger : Logger, version : ApiVersion) =
    let (PointName defaultName) = logger.name
    let createMessage = LogMessage.create loggerType

    let toMessageFactory (oLevel : obj) (facadeFactory : obj) : LogLevel -> Message =
      let typ = facadeFactory.GetType()
      let invokeMethod = findMethod (typ, "Invoke")
      fun level ->
        // Here we actually create the instance
        let message = createMessage defaultName oLevel
        toMessage defaultName (invokeMethod.Invoke(facadeFactory, [| box message |]))

    let log level messageFactory ct =
      Alt.toTask ct (logger.log level messageFactory)

    let logWithAck level messageFactory ct =
      Alt.toTask ct (logger.logWithAck level messageFactory)

    interface IInterceptor with
      member x.Intercept invocation =
        let oLevel = invocation.Arguments.[0]
        let level = toLogLevel oLevel
        let factory = toMessageFactory oLevel invocation.Arguments.[1]
        let ct = invocation.Arguments.[2] // maybe null cancellation token
        match invocation.Method.Name with
        | "Log" ->
          invocation.ReturnValue <- log level factory (ct :?> CancellationToken)
        | "LogWithAck" ->
          invocation.ReturnValue <- logWithAck level factory (ct :?> CancellationToken)
        | meth ->
          failwithf "Method '%s' should not exist on Logary.CSharp.Facade.ILogger" meth

  [<CompiledName "Create">]
  let create (typ : Type) logger : obj =
    if typ = null then invalidArg "typ" "is null"
    let generator = new ProxyGenerator()
    let facade = I (typ, logger, ApiVersion.ofType typ) :> IInterceptor
    generator.CreateInterfaceProxyWithoutTarget(typ, facade)

  /// Create a target assembly's logger from the given type-string, which
  /// delegates to the passed Logary proper logger.
  [<CompiledName "Create">]
  let createString (typ : string) logger : obj =
    create (Type.GetType typ) logger

  /// Creates a target assembly's logger from the passed generic logger type
  /// which delegates to the passed Logary-proper's logger. This is the function
  /// you'll normally use if you use this module. Otherwise, please see
  /// LogaryFacadeAdapter.
  [<CompiledName "Create">]
  let createGeneric<'logger when 'logger : not struct> logger : 'logger =
    create typeof<'logger> logger :?> 'logger

/// An adapter for creating a `getLogger` function.
module LogaryFacadeAdapter =
  open Reflection

  let internal (|GetLogger|GetTimestamp|ConsoleSemaphore|) (i : IInvocation) =
    match i.Method.Name with
    | "GetTimestamp" ->
      GetTimestamp
    | "GetLogger" ->
      GetLogger (i.Arguments.[0] :?> string[])
    | "get_ConsoleSemaphore" ->
      ConsoleSemaphore
    | otherwise ->
      failwithf "Unknown function called '%s'" otherwise

  type internal LoggerConfigImpl(loggerType : Type, logManager : LogManager) =
    interface IInterceptor with
      member x.Intercept invocation =
        match invocation with
        | GetLogger name ->
          invocation.ReturnValue <-
            PointName name
            |> logManager.getLogger
            |> LoggerCSharpAdapter.create loggerType
        | GetTimestamp ->
          invocation.ReturnValue <- Logary.Internals.Global.getTimestamp ()
        | ConsoleSemaphore ->
          invocation.ReturnValue <- Logary.Internals.Global.getConsoleSemaphore ()

  /// You should probably gaze at `initialise` rather than this function. This
  /// function creates a configuration matching the `configType`; and it also
  /// needs the `loggerType` of the target assembly for orientation.
  [<CompiledName "CreateFSharpConfig">]
  let createConfig configType loggerType (logManager : LogManager) =
    let values : obj array =
      FSharpType.GetRecordFields(configType)
      |> Array.map (fun field ->
        match field.Name with
        | "getLogger" ->
          // getLogger on LogManager will return a PromisedLogger, which
          // will have its value set as the promise returns; this is on the
          // order of milliseconds
          FSharpValue.MakeFunction(
            field.PropertyType,
            (unbox >> fun name ->
              PointName name
              |> logManager.getLogger
              |> LoggerAdapter.create loggerType))

        | "timestamp" ->
          FSharpValue.MakeFunction(
            field.PropertyType,
            fun _ -> Logary.Internals.Global.getTimestamp () |> box)

        | "consoleSemaphore" ->
          Logary.Internals.Global.getConsoleSemaphore ()

          // If you want to try this file in the interactive...
          //obj()

        | name ->
          failwithf "Unknown field '%s' of the config record '%s'" name configType.FullName)

    FSharpValue.MakeRecord(configType, values)

  [<CompiledName "CreateCSharpConfig">]
  let createCSharpConfig configType loggerType logManager =
    let generator = new ProxyGenerator()
    let target = LoggerConfigImpl(loggerType, logManager) :> IInterceptor
    generator.CreateInterfaceProxyWithoutTarget(configType, target)

  /// Initialises the global state in the target assembly, but calling
  /// YourAssembly.Logging.Global.initialise with a configuration value which
  /// is pointing to Logary.
  [<CompiledName "Initialise">]
  let initialise<'logger> (logManager : LogManager) : unit =
    let loggerType = typeof<'logger>
    match typeof<'logger> with
    | FSharp (version, globalType, configType) ->
      //printfn "====> Matched version=%O, Global=%O, LoggingConfig=%O" version globalType configType
      let fn = findMethod (globalType, "initialise")
      let cfg = createConfig configType loggerType logManager
      fn.Invoke(null, [| cfg |]) |> ignore

    | CSharp (version, globalType, configType) ->
      let fn = findMethod (globalType, "Initialise")
      let cfg = createCSharpConfig configType loggerType logManager
      fn.Invoke(null, [| cfg |]) |> ignore