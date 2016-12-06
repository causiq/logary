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

/// Utilities for creating a single 'MyLib.Logging.Logger' in the target type
/// space.
module LoggerAdapter =

  let private versionFrom_ (loggerType : Type) =
    let typ = sprintf "%s.Literals, %s" loggerType.Namespace loggerType.Assembly.FullName
    let literals = Type.GetType typ
    let field = literals.GetProperty("FacadeVersion", BindingFlags.Static ||| BindingFlags.Public)
    //printfn "=> versionFrom_, field: %A" field
    if isNull field then 1u
    else field.GetValue(null, null) :?> uint32

  let private versionFrom =
    Cache.memoize versionFrom_

  type internal ApiVersion =
    /// This API version did not have an explicit version field.
    | V1
    /// NS.Literals.FacadeVersion
    | V2

    static member ofType (loggerType : Type) =
      match versionFrom loggerType with
      | 2u ->
        V2
      | _ ->
        V1

  let private findMethod : Type * string -> MethodInfo =
    Cache.memoize (fun (typ, meth) -> typ.GetMethod meth)

  let private findProperty : Type * string -> PropertyInfo =
    Cache.memoize (fun (typ, prop) -> typ.GetProperty prop)

  let private defaultName (fallback : string[]) = function
    | [||] ->
      fallback
    | otherwise ->
      otherwise

  /// Convert the object instance to a PointValue. Is used from the
  /// other code in this module.
  let toPointValue (o : obj) : PointValue =
    let typ = o.GetType()
    let info, values = FSharpValue.GetUnionFields(o, typ)
    match info.Name, values with
    | "Event", [| template |] ->
      Event (template :?> string)

    | "Gauge", [| value; units |] ->
      Gauge (Int64 (value :?> int64), Units.Scalar)

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

    { name      = PointName (readProperty "name" :?> string [] |> defaultName fallbackName)
      value     = readProperty "value" |> toPointValue
      fields    = Map.empty
      context   = Map.empty
      timestamp = readProperty "timestamp" :?> EpochNanoSeconds
      level     = readProperty "level" |> toLogLevel }
    |> Message.setFieldsFromMap (readProperty "fields" :?> Map<string, obj>)

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

    let rawPrint (invocation : IInvocation) =
      printfn "Invocation"
      printfn "=========="
      printfn "Args: "
      for arg in invocation.Arguments do
        let argType = arg.GetType()
        printfn " - %A\n   : %s" arg (argType.FullName)
        printfn "   :> %s" argType.BaseType.FullName
      printfn "Method.Name: %s" invocation.Method.Name

    interface IInterceptor with
      member x.Intercept invocation =
        //rawPrint invocation
        match invocation, defaultName with
        | Log (level, messageFactory) when version = V1 ->
          let ret = logV1 level messageFactory
          //printfn "Ret (V1): %A" ret
          invocation.ReturnValue <- ret
        
        | Log (level, messageFactory) ->
          let ret = logV2 level messageFactory
          //printfn "Ret (V2): %A" ret
          invocation.ReturnValue <- ret

        | LogWithAck (level, messageFactory) ->
          invocation.ReturnValue <- logWithAck level messageFactory

        | LogSimple message ->
          invocation.ReturnValue <- logSimple message

  /// Create a target assembly's logger from the given type, which delegates to
  /// the passed Logary proper Logger.
  let create (typ : Type) logger : obj =
    if typ = null then invalidArg "typ" "is null"
    let generator = new ProxyGenerator()
    let facade = I (logger, ApiVersion.ofType typ) :> IInterceptor
    generator.CreateInterfaceProxyWithoutTarget(typ, facade)

  /// Create a target assembly's logger from the given type-string, which
  /// delegates to the passed Logary proper logger.
  let createString (typ : string) logger : obj =
    create (Type.GetType typ) logger

  /// Creates a target assembly's logger from the passed generic logger type
  /// which delegates to the passed Logary-proper's logger. This is the function
  /// you'll normally use if you use this module. Otherwise, please see
  /// LogaryFacadeAdapter.
  let createGeneric<'logger when 'logger : not struct> logger : 'logger =
    create typeof<'logger> logger :?> 'logger

/// An adapter for creating a `getLogger` function.
module LogaryFacadeAdapter =

  /// You should probably gaze at `initialise` rather than this function. This
  /// function creates a configuration matching the `configType`; and it also
  /// needs the `loggerType` of the target assembly for orientation.
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
            fun _ -> Date.timestamp () |> box)

        | "consoleSemaphore" ->
          Logary.Internals.Globals.consoleSemaphore

        | name ->
          failwithf "Unknown field '%s' of the config record '%s'" name configType.FullName)

    FSharpValue.MakeRecord(configType, values)

  /// Initialises the global state in the target assembly, but calling
  /// YourAssembly.Logging.Global.initialise with a configuration value which
  /// is pointing to Logary.
  let initialise<'logger> (logManager : LogManager) : unit =
    let loggerType = typeof<'logger>
    let asm = loggerType.Assembly
    let ns = loggerType.Namespace
    let configType = Type.GetType(ns + ".LoggingConfig, " + asm.FullName)
    let globalType = Type.GetType(ns + ".Global, " + asm.FullName)
    let fn = globalType.GetMethod("initialise")
    let cfg = createConfig configType loggerType logManager
    fn.Invoke(null, [| cfg |]) |> ignore