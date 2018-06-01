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
open Logary.Configuration

module Reflection =

  /// Gets the method on the type; the strings are part of the cache key
  let findMethod: Type * string -> MethodInfo =
    Cache.memoize (fun (typ, meth) -> typ.GetMethod meth)

  /// Gets the method on the type; the strings are part of the cache key
  let findStaticMethod: Type * string -> MethodInfo =
    Cache.memoize (fun (typ, meth) -> typ.GetMethod(meth, BindingFlags.Static ||| BindingFlags.NonPublic))

  /// Gets the property on the type
  let findProperty: Type * string -> PropertyInfo =
    Cache.memoize (fun (typ, prop) -> typ.GetProperty prop)

  let findStaticProperty: Type * string -> PropertyInfo =
    Cache.memoize (fun (typ, prop) -> typ.GetProperty(prop, BindingFlags.Static ||| BindingFlags.Public ||| BindingFlags.NonPublic))

  let findField: Type * string -> FieldInfo =
    Cache.memoize (fun (typ, field) -> typ.GetField field)

  /// Finds the module relative to the logger type
  let findModule =
    let findModule_: Type * string -> Type =
      fun (loggerType, moduleName) ->
        let typ = sprintf "%s.%s, %s" loggerType.Namespace moduleName loggerType.Assembly.FullName
        Type.GetType typ
    Cache.memoize findModule_

  let readLiteral (loggerType: Type) =
    Cache.memoize (fun (propertyName: string, defaultValue : 'a) ->
      let literals = findModule (loggerType, "Literals")
      let field = literals.GetProperty(propertyName, BindingFlags.Static ||| BindingFlags.Public)
      if isNull field then defaultValue
      else field.GetValue(null, null) :?> 'a)

  let versionFrom: Type -> uint32 =
    Cache.memoize (fun loggerType -> readLiteral loggerType ("FacadeVersion", 1u))

  [<CustomComparison; CustomEquality>]
  type ApiVersion =
    /// This API version did not have an explicit version field.
    | V1
    /// NS.Literals.FacadeVersion
    | V2
    /// NS.Literals.FacadeVersion (Gauge is float)
    | V3
    // NS.Literals.FacadeVersion (Async -> Hopac, timing functions, Units etc)
    | V4
    override x.ToString() =
      match x with
      | V1 -> "V1"
      | V2 -> "V2"
      | V3 -> "V3"
      | V4 -> "V4"
    static member ofType (loggerType: Type) =
      match versionFrom loggerType with
      | 4u ->
        V4
      | 3u ->
        V3
      | 2u ->
        V2
      | _ ->
        V1
    member x.intValue =
      match x with
      | V1 -> 1
      | V2 -> 2
      | V3 -> 3
      | V4 -> 4
    interface IComparable with
      member x.CompareTo other =
        match other with
        | :? ApiVersion as vOther ->
          x.intValue.CompareTo vOther.intValue
        | _ ->
          1
    override x.Equals other =
      match other with
      | :? ApiVersion as vOther ->
        vOther.intValue = x.intValue
      | _ ->
        false
    override x.GetHashCode() =
      x.intValue.GetHashCode()
    static member op_LessThan (v1: ApiVersion, v2: ApiVersion) =
      v1.intValue < v2.intValue
    static member op_GreaterThan (v1: ApiVersion, v2: ApiVersion) =
      v1.intValue > v2.intValue
    static member op_LessThanOrEqual (v1: ApiVersion, v2: ApiVersion) =
      v1.intValue <= v2.intValue
    static member op_GreaterThanOrEqual (v1: ApiVersion, v2: ApiVersion) =
      v1.intValue >= v2.intValue

  let (|FSharp|CSharp|) (loggerType: Type) =
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

  let rawPrint (invocation: IInvocation) =
    printfn "Invocation"
    printfn "=========="
    printfn "Args: "
    for arg in invocation.Arguments do
      let argType = arg.GetType()
      printfn " - %A\n   : %s" arg (argType.FullName)
      printfn "   :> %s" argType.BaseType.FullName
    printfn "Method.Name: %s" invocation.Method.Name
    printfn "----------"

  let rawPrintM (method: MethodInfo) (args: obj[]) =
    printfn "Target method: %A" method
    printfn "============="
    printfn "Parameters: "
    for arg in method.GetParameters() do
      let argType = arg.GetType()
      printfn " - %A" arg
    printfn "Method.Name: %s" method.Name
    printfn "---------"
    printfn "Argument values:"
    printfn "----------------"
    for arg in args do
      let argType = arg.GetType()
      printfn " - %A  (%s)" arg argType.FullName
      printfn "   :> %s" argType.BaseType.FullName
    printfn "----------"

module LoggerAdapterShared =
  /// temporary solution
  type OldPointValue =
    | Event of string
    | Gauge of float * Units

module List =
  let pickOff picker l =
    let rec loop picked left =
      function
      | [] ->
        List.rev picked, List.rev left

      | x :: xs ->
        match picker x with
        | None ->
          loop picked (x::left) xs

        | Some p ->
          loop (p::picked) left xs

    loop [] [] l

/// Utilities for creating a single 'MyLib.Logging.Logger' in the target type
/// space. The original logger adapter (also see the LoggerCSharpAdapter further
/// below)
module LoggerAdapter =
  open System.Collections.Generic
  open Reflection

  let private castDefault<'a> (fallback: 'a) (x: obj): 'a =
    match x with
    | :? 'a as a -> a
    | _ -> fallback

  let private tryCast<'a> (x: obj): 'a option =
    match x with
    | :? 'a as a -> Some a
    | _ -> None

  let private defaultName (fallback: string[]) = function
    | [||] ->
      fallback
    | otherwise ->
      otherwise

  let strToUnits (x: obj) =
    match x with
    | :? string as s ->
      Units.parse s
    | _ ->
      Units.Scalar

  /// Convert the object instance to a PointValue. Is used from the
  /// other code in this module.
  let toPointValue (v: ApiVersion) (o: obj): LoggerAdapterShared.OldPointValue =
    let typ = o.GetType()
    let info, values = FSharpValue.GetUnionFields(o, typ)
    match info.Name, values with
    | "Event", [| template |] ->
      LoggerAdapterShared.OldPointValue.Event (template :?> string)

    | "Gauge", [| value; units |] when v <= ApiVersion.V2 ->
      LoggerAdapterShared.OldPointValue.Gauge (float (value :?> int64), strToUnits units)

    | "Gauge", [| value; units |] ->
      LoggerAdapterShared.OldPointValue.Gauge (float (value :?> float), strToUnits units)

    | caseName, values ->
      let valuesStr = values |> Array.map string |> String.concat ", "
      failwithf "Unknown union case '%s (%s)' on '%s'" caseName valuesStr typ.FullName

  /// Given an object (a boxed LogLevel type in the using assembly's type-space)
  /// gives back a memoized function that looks up the LogLevel type in the using assembly's type-space.
  ///
  /// Saves the call to `logLevel.GetType()`, as we already know the logger type at the time of generating the logger.
  ///
  /// The LogLevel value is NOT part of the cache key; only the logger type is.
  ///
  /// Returns `level:LogLevel -> loggerType:Type -> logLevelType:Type
  let logLevelTypeOf: obj -> Type -> Type =
    // NOT HOT PATH
    let memoize = Cache.memoizeFactory (ConcurrentDictionary<_,_>()) // expensive call
    fun (logLevel: obj) ->
      // HOT PATH, cheap call
      memoize (fun _ ->
        // NOT HOT PATH, expensive call; called once per adapter assembly load, per adapted library
        logLevel.GetType())

  let messageTypeOf: obj -> Type -> Type =
    let memoize = Cache.memoizeFactory (ConcurrentDictionary<_,_>())
    fun (message: obj) -> memoize (fun _ -> message.GetType())
  let gaugeTypeOf: obj -> Type -> Type =
    let memoize = Cache.memoizeFactory (ConcurrentDictionary<_,_>())
    fun (gauge: obj) -> memoize (fun _ -> gauge.GetType())
  let unitTypeOf: obj -> Type -> Type =
    let memoize = Cache.memoizeFactory (ConcurrentDictionary<_,_>())
    fun (units: obj) -> memoize (fun _ -> units.GetType())
  let valueTypeOf: obj -> Type -> Type =
    let memoize = Cache.memoizeFactory (ConcurrentDictionary<_,_>())
    fun (value: obj) -> memoize (fun _ -> value.GetType())

  /// Convert the object instance to a LogLevel. Is used from the
  /// other code in this module.
  let toLogLevel loggerType (o: obj): LogLevel =
    // HOT PATH
    let logLevelType = logLevelTypeOf o loggerType
    let toInt = findMethod (logLevelType, "toInt")
    LogLevel.ofInt (toInt.Invoke(o, null) :?> int)

  /// Convert the object instance to a Logary.DataModel.Message. Is used from the
  /// other code in this module.
  let toMsgV3 (v: ApiVersion) (loggerType, fallbackName) (o: obj): Message =
    if v > V3 then invalidOp "This function only works for <= V3 facades"
    let messageType = messageTypeOf o loggerType
    let readProperty name = (findProperty (messageType, name)).GetValue o
    let oldPointValue = readProperty "value" |> toPointValue v
    let event =
      match oldPointValue with
      | LoggerAdapterShared.OldPointValue.Event tpl -> tpl
      | _ -> String.Empty

    let fields, exns =
      let m = readProperty "fields" |> castDefault<Map<string, obj>> Map.empty
      match m |> Map.tryFind "errors" with
      | None ->
        m, []
      | Some (:? list<obj> as xs) ->
        let ours, theirs = List.pickOff tryCast<exn> xs
        m |> Map.add "errors" (box theirs), ours
      | _ ->
        m, []

    let pointName =
      readProperty "name"
      |> castDefault<string []> [||]
      |> defaultName fallbackName

    { name      = PointName pointName
      value     = event
      context   = HashMap.empty
      timestamp = readProperty "timestamp" |> castDefault<EpochNanoSeconds> 0L
      level     = readProperty "level" |> toLogLevel loggerType }
    |> Message.setFieldsFromMap fields
    |> Message.addExns exns
    |> (fun msg ->
        match oldPointValue with
        | LoggerAdapterShared.OldPointValue.Gauge (value, units) ->
          let g = Gauge (Float value, units)
          let gaugeName =
            match pointName.Length with
            | 0 -> KnownLiterals.DefaultGaugeName
            | n -> pointName.[n-1]
          msg |> Message.addGauge gaugeName g
        | _ ->
          msg)

  (* Sanity check; shadows 'unitsTypeOf'
  let unitsTypeOf (o: obj) (loggerType: Type): Type =
    let cachedType = unitTypeOf o loggerType
    let unitsType = o.GetType()
    if cachedType <> unitsType then
      failwithf "Cached unit type: %s, actual unit type: %s, logger type: %s"
                cachedType.FullName unitsType.FullName loggerType.FullName
    cachedType
  *)

  let rec toUnits (loggerType: Type) (o: obj): Units =
    let unitsType = unitTypeOf o loggerType
    let tag = findProperty(unitsType, "Tag").GetValue o :?> int
    //printfn "Value=%A, Tag=%i" o tag
    match tag with
    | 0 ->
      let u = findProperty(unitsType, "scaledUnit").GetValue o
      let v = findProperty(unitsType, "scaledFloat").GetValue o :?> float
      Units.Scaled (toUnits loggerType u, v)
    | 1 ->
      Units.Seconds
    | 2 ->
      Units.Scalar
    | 3 ->
      let oP = findProperty(unitsType, "otherUnit")
      //printfn "property: %O, properties: %A, fields: %A" oP (unitsType.GetProperties()) (unitsType.GetFields())
      // seems this property is not stable:
      if isNull oP then Units.Scalar else
      let unitString = oP.GetValue o
      strToUnits unitString
    | other ->
      failwithf "Unknown tag %i, for %O => %A" other unitsType o

  let toValue (loggerType: Type) (o: obj): Value =
    let valueType = valueTypeOf o loggerType
    let toFloatMethod = findMethod (valueType, "toFloat")
    let floatValue = toFloatMethod.Invoke(o, [||])
    Float (floatValue :?> float)

  let toGauge (loggerType: Type) (o: obj): Gauge =
    let gaugeType = gaugeTypeOf o loggerType
    let unit = findProperty(gaugeType, "unit").GetValue o
    let value = findProperty(gaugeType, "value").GetValue o
    Gauge (toValue loggerType value, toUnits loggerType unit)

  let toContext loggerType (context: Map<string, obj>): HashMap<string, obj> =
    context
    |> Seq.map (fun (KeyValue (k, v)) ->
      if k.StartsWith KnownLiterals.GaugeNamePrefix then
        KeyValuePair (k, box (toGauge loggerType v))
      else
        KeyValuePair (k, v))
    |> HashMap.ofSeqPair

  // NOT HOT PATH
  let toMsgV4 (loggerType, fallbackName) (o: obj): Message =
    // HOT PATH
    let messageType = messageTypeOf o loggerType
    let readProperty name = (findProperty (messageType, name)).GetValue o
    { name = readProperty "name" |> castDefault<string []> [||] |> defaultName fallbackName |> PointName
      value = readProperty "value" |> castDefault<string> ""
      // TO CONSIDER: optimising creating a HashMap of a Map:
      context = readProperty "context" |> castDefault<Map<string, obj>> Map.empty |> toContext loggerType
      level = readProperty "level" |> toLogLevel loggerType
      timestamp = readProperty "timestamp" |> castDefault<EpochNanoSeconds> 0L
    }

  /// Convert the object instance to a message factory method. Is used from the
  /// other code in this module.
  let toMsgFactory (v: ApiVersion) (loggerType, fallbackName) oLevel (fsFunc: obj): LogLevel -> Message =
    // HOT PATH
    let fsFuncType = fsFunc.GetType ()
    //                messageFactoryTypeOf fsFunc loggerType (using this cached variant crashes)
    let messageFactory = findMethod (fsFuncType, "Invoke")
    if v <= V3 then
      fun level ->
        //do rawPrintM messageFactory [| oLevel; level |]
        toMsgV3 v (loggerType, fallbackName) (messageFactory.Invoke(fsFunc, [| oLevel |]))
    else
      // TO CONSIDER: how much GC does returning this function generate?
      fun level ->
        //do rawPrintM messageFactory [| oLevel; level |]
        toMsgV4 (loggerType, fallbackName) (messageFactory.Invoke(fsFunc, [| oLevel |]))

  module LogResult =
    /// Homomorphism on the LogResult value.
    let mapErr<'err> (onFull: string -> 'err) (onRejected: 'err) (xA: LogResult) =
      xA ^-> function
        | Ok ack ->
          Ok ack
        | Result.Error (BufferFull target) ->
          Result.Error (onFull target)
        | Result.Error Rejected ->
          Result.Error onRejected

    let createMapErr =
      let mapErrMethodModule = Type.GetType "Logary.Adapters.Facade.LoggerAdapter+LogResult, Logary.Adapters.Facade"
      let mapErrMethodType = mapErrMethodModule.GetMethod("mapErr", BindingFlags.Public ||| BindingFlags.Static)
      fun (loggerType: Type) ->
        let logErrorType = findModule (loggerType, "LogError")
        mapErrMethodType.MakeGenericMethod [| logErrorType |]

  let ofLogResult (loggerType: Type): LogResult -> obj =
    // HOT PATH
    let logErrorType = findModule (loggerType, "LogError")
    let logErrorModule = findModule (loggerType, "LogErrorModule")
    let logResultModule = findModule (loggerType, "LogResultModule")

    let bufferFullMethod = findStaticMethod (logResultModule, "bufferFull")
    let bufferFullFSharpFunction = typedefof<FSharpFunc<_,_>>.MakeGenericType([| typeof<string>; logErrorType |])
    let bufferFull =
      FSharpValue.MakeFunction(
        bufferFullFSharpFunction,
        fun (target: obj) -> bufferFullMethod.Invoke(null, [| target |]))

    let rejectedValue = findStaticProperty(logErrorModule, "rejected").GetValue(null, null)
    let mapErr = LogResult.createMapErr loggerType
    fun (result: LogResult) ->
      let args = [| bufferFull; rejectedValue; box result |]
      // do rawPrintM mapErr args
      mapErr.Invoke(null, args)

  let internal (|LogWithAck|LogWithAckV3|Log|LogSimple|)
               (invocation: IInvocation, defaultName: string[], v: ApiVersion, loggerType: Type)
               : Choice<(LogResult -> obj) * (bool * LogLevel) * (LogLevel -> Message),
                         _, _, _> =
    // HOT PATH
    match invocation.Method.Name with
    | "logWithAck" when v = V4 ->
      let wait = invocation.Arguments.[0] :?> bool
      let oLevel = invocation.Arguments.[1]
      let level = toLogLevel loggerType oLevel
      let factory = toMsgFactory v (loggerType, defaultName) oLevel invocation.Arguments.[2]
      let mapResult = ofLogResult loggerType
      LogWithAck (mapResult, (wait, level), factory)

    | "logWithAck" when v <= V3 ->
      let oLevel = invocation.Arguments.[0]
      let level = toLogLevel loggerType oLevel
      let factory = toMsgFactory v (loggerType, defaultName) oLevel invocation.Arguments.[1]
      LogWithAckV3 (level, factory)

    | "log" when v <= V3 ->
      let oLevel = invocation.Arguments.[0]
      let level = toLogLevel loggerType oLevel
      let factory = toMsgFactory v (loggerType, defaultName) oLevel invocation.Arguments.[1]
      Log (level, factory)

    | "logSimple" when v <= V1 ->
      let msg = toMsgV3 v (loggerType, defaultName) invocation.Arguments.[0]
      LogSimple msg

    | meth ->
      failwithf "Method '%s' should not exist on Logary.Facade.Logger, with type '%O' @ %A." meth loggerType v

  /// This is the main adapter which logs from an arbitrary logary facade
  /// into logary. Provide the namespace you put the facade in and the assembly
  /// which it should be loaded from, and this adapter will use (memoized) reflection
  /// to properly bind to the facade.
  type private I(logger: Logger, version: ApiVersion, loggerType: Type) =
    // NOT HOT PATH
    let (PointName defaultName) = logger.name

    // Codomains of these three functions are equal to codomains of Facade's
    // functions:

    let logWithAckV3 (level: LogLevel) (messageFactory: LogLevel -> Message): Async<unit> =
      // a placeholder for the Promise ack from Logary proper
      let prom = IVar ()

      // kick off the logging no matter what
      start (logger.logWithAck (false, level) messageFactory ^=> IVar.fill prom)

      // take the promise from within the IVar and make it an Async (which is
      // "hot" in that starting it will return "immediately" and be idempotent)
      (prom ^=> function
        | Ok ack ->
          ack
        | Result.Error err ->
          Promise (()))
      |> Job.toAsync

    /// v1
    let logSimple (msg: Message): unit =
      logger.logSimple msg

    let logV1 level messageFactory: unit =
      // start immediate because in the normal case we can put the Message
      // in the RingBuffer without any extra time taken
      logWithAckV3 level messageFactory |> Async.StartImmediate

    let logV2 level messageFactory: Async<unit> =
      logger.logWithBP level messageFactory |> Alt.toAsync

    // NOT HOT PATH
    /// v4+
    let logWithAck (mapResult: LogResult -> (* facade-typed result *) obj)
                   (waitForBuffers: bool, level: LogLevel) (messageFactory: LogLevel -> Message): obj =
      // HOT PATH
      logger.logWithAck (waitForBuffers, level) messageFactory
      |> mapResult

    interface IInterceptor with
      member x.Intercept invocation =
        // HOT PATH
        match invocation, defaultName, version, loggerType with
        | LogWithAck (mapResult, (wait, level), messageFactory) ->
          invocation.ReturnValue <- logWithAck mapResult (wait, level) messageFactory

        | LogWithAckV3 (level, messageFactory) ->
          invocation.ReturnValue <- logWithAckV3 level messageFactory

        | Log (level, messageFactory) when version = V1 ->
          let ret = logV1 level messageFactory
          invocation.ReturnValue <- ret

        | Log (level, messageFactory) ->
          let ret = logV2 level messageFactory
          invocation.ReturnValue <- ret

        | LogSimple message ->
          invocation.ReturnValue <- logSimple message

  /// Create a target assembly's logger from the given type, which delegates to
  /// the passed Logary proper Logger.
  [<CompiledName("Create")>]
  let create (loggerType: Type) logger: obj =
    if loggerType = null then invalidArg "typ" "is null"
    let generator = new ProxyGenerator()
    let facade = I (logger, ApiVersion.ofType loggerType, loggerType) :> IInterceptor
    generator.CreateInterfaceProxyWithoutTarget(loggerType, facade)

  /// Create a target assembly's logger from the given type-string, which
  /// delegates to the passed Logary proper logger.
  [<CompiledName("Create")>]
  let createString (loggerType: string) logger: obj =
    create (Type.GetType loggerType) logger

  /// Creates a target assembly's logger from the passed generic logger type
  /// which delegates to the passed Logary-proper's logger. This is the function
  /// you'll normally use if you use this module. Otherwise, please see
  /// LogaryFacadeAdapter.
  [<CompiledName("Create")>]
  let createGeneric<'logger when 'logger: not struct> logger : 'logger =
    create typeof<'logger> logger :?> 'logger

module LoggerCSharpAdapter =
  open System.Threading
  open System.Threading.Tasks
  open Reflection
  open Logary.CSharp

  let private defaultName (fallback: string[]) = function
    | [||] ->
      fallback
    | otherwise ->
      otherwise

  let internal toPointValue (o: obj): LoggerAdapterShared.OldPointValue =
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
      let converted = Units.parse unitValue
      LoggerAdapterShared.OldPointValue.Gauge (float valueValue, converted)
    else
      failwithf "Unknown point value type name '%s'" typ.Name

  let internal toLogLevel (o: obj): LogLevel =
    LogLevel.ofInt (unbox (Convert.ChangeType (o, typeof<int>)))

  /// Convert the object instance to a Logary.DataModel.Message. Is used from the
  /// other code in this module.
  let internal toMessage fallbackName (o: obj): Message =
    let typ = o.GetType()
    let readProperty name = (findProperty (typ, name)).GetValue o
    let oldPointValue = readProperty "Value" |> toPointValue
    let event =
      match oldPointValue with
      | LoggerAdapterShared.OldPointValue.Event tpl -> tpl
      | _ -> String.Empty
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
          let g = Gauge (Float value, units)
          msg |> Message.addGauge KnownLiterals.DefaultGaugeName g
        | _ -> msg)

  module internal LogMessage =
    let create (loggerType: Type): string[] -> obj -> obj =
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
      let pointValueType = findModule (loggerType, "PointValue")
      let emptyValueProp = findStaticProperty (pointValueType, "Empty")
      let emptyValue = emptyValueProp.GetValue(null, null)

      fun (name: string[]) (level: obj) ->
        let ts =  Logary.Internals.Global.getTimestamp ()
        let args = [| box name; level; emptyValue; box emptyDic; box ts |]
        //printfn "====> %s(%A)" logMessageT.Name args
        Activator.CreateInstance(logMessageT, args)

  type private I (loggerType: Type, logger: Logger, version: ApiVersion) =
    let (PointName defaultName) = logger.name
    let createMessage = LogMessage.create loggerType

    let toMessageFactory (oLevel: obj) (facadeFactory: obj): LogLevel -> Message =
      let typ = facadeFactory.GetType()
      let invokeMethod = findMethod (typ, "Invoke")
      fun level ->
        // Here we actually create the instance
        let message = createMessage defaultName oLevel
        toMessage defaultName (invokeMethod.Invoke(facadeFactory, [| box message |]))

    let log level messageFactory ct =
      Alt.toTask ct (logger.log level messageFactory)

    let logWithAck level messageFactory ct =
      Alt.toTask ct (logger.logWithAck (true, level) messageFactory)

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
  let create (typ: Type) logger: obj =
    if typ = null then invalidArg "typ" "is null"
    let generator = new ProxyGenerator()
    let facade = I (typ, logger, ApiVersion.ofType typ) :> IInterceptor
    generator.CreateInterfaceProxyWithoutTarget(typ, facade)

  /// Create a target assembly's logger from the given type-string, which
  /// delegates to the passed Logary proper logger.
  [<CompiledName "Create">]
  let createString (typ: string) logger: obj =
    create (Type.GetType typ) logger

  /// Creates a target assembly's logger from the passed generic logger type
  /// which delegates to the passed Logary-proper's logger. This is the function
  /// you'll normally use if you use this module. Otherwise, please see
  /// LogaryFacadeAdapter.
  [<CompiledName "Create">]
  let createGeneric<'logger when 'logger: not struct> logger : 'logger =
    create typeof<'logger> logger :?> 'logger

/// An adapter for creating a `getLogger` function.
module LogaryFacadeAdapter =
  open Reflection

  let internal (|GetLogger|GetTimestamp|ConsoleSemaphore|) (i: IInvocation) =
    match i.Method.Name with
    | "GetTimestamp" ->
      GetTimestamp
    | "GetLogger" ->
      GetLogger (i.Arguments.[0] :?> string[])
    | "get_ConsoleSemaphore" ->
      ConsoleSemaphore
    | otherwise ->
      failwithf "Unknown function called '%s'" otherwise

  type internal LoggerConfigImpl(loggerType: Type, logManager: LogManager) =
    interface IInterceptor with
      member x.Intercept invocation =
        match invocation with
        | GetLogger name ->
          logManager.runtimeInfo.logger.debug (Message.eventX "getLogger invoked with {name}" >> Message.setField "name" name)
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
  let createFSharpConfig configType loggerType (logManager: LogManager) =
    let values: obj array =
      FSharpType.GetRecordFields configType
      |> Array.map (fun field ->
        match field.Name with
        | "getLogger" ->
          // getLogger on LogManager will return a PromisedLogger, which
          // will have its value set as the promise returns; this is on the
          // order of milliseconds
          FSharpValue.MakeFunction(
            field.PropertyType,
            (unbox >> fun name ->
              // Can be used to track if there are too many `Log.create` calls; these will cause a large number of dynamic
              // proxies to be generated.
              logManager.runtimeInfo.logger.debug (Message.eventX "getLogger invoked with {name}" >> Message.setField "name" name)
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
          failwithf "Unknown field '%s' of the config record '%s'." name configType.FullName)

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
  let initialise<'logger> (logManager: LogManager): unit =
    match typeof<'logger> with
    | FSharp (version, globalType, configType) as loggerType ->
      //printfn "====> Matched version=%O, Global=%O, LoggingConfig=%O" version globalType configType
      let fn = findMethod (globalType, "initialise")
      let cfg = createFSharpConfig configType loggerType logManager
      fn.Invoke(null, [| cfg |]) |> ignore

    | CSharp (version, globalType, configType) as loggerType ->
      let fn = findMethod (globalType, "Initialise")
      let cfg = createCSharpConfig configType loggerType logManager
      fn.Invoke(null, [| cfg |]) |> ignore