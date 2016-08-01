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

module LogaryFacadeAdapter =
  let private findMethod : Type * string -> MethodInfo =
    Cache.memoize (fun (typ, meth) -> typ.GetMethod meth)

  let private findProperty : Type * string -> PropertyInfo =
    Cache.memoize (fun (typ, prop) -> typ.GetProperty prop)

  let defaultName (fallback : string[]) = function
    | [||] ->
      fallback

    | otherwise ->
      otherwise

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

  let toLogLevel (o : obj) : LogLevel =
    let typ = o.GetType()
    let par = typ, "toInt"
    let toInt = findMethod (typ, "toInt")
    LogLevel.ofInt (toInt.Invoke(o, null) :?> int)

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

  let toMsgFactory fallbackName (o : obj) : unit -> Message =
    let typ = o.GetType()
    let invokeMethod = findMethod (typ, "Invoke")
    fun () -> toMsg fallbackName (invokeMethod.Invoke(o, [| () |]))

  let (|Log|LogSimple|) ((invocation, defaultName) : IInvocation * string[]) : Choice<_, _> =
    match invocation.Method.Name with
    | "log" ->
      let level = toLogLevel invocation.Arguments.[0]
      let factory = toMsgFactory defaultName invocation.Arguments.[1]
      Log (level, factory)

    | "logSimple" ->
      let msg = toMsg defaultName invocation.Arguments.[0]
      LogSimple msg

    | meth ->
      failwithf "Method '%s' should not exist on Logary.Facade.Logger" meth

  /// This is the main adapter which logs from an arbitrary logary facade
  /// into logary. Provide the namespace you put the facade in and the assembly
  /// which it should be loaded from, and this adapter will use (memoized) reflection
  /// to properly bind to the facade.
  type private I(logger : Logger) =
    let (PointName defaultName) = logger.name

    // Codomains of these two functions are equal to codomains of Facade's
    // functions:

    let log (level : LogLevel) (msgFactory : unit -> Message) : Async<unit> =
      if logger.level <= level then
        let prom = IVar ()

        // kick off the logging no matter what
        let message = msgFactory ()

        start (Logger.logWithAck logger message ^=> IVar.fill prom)

        // take the promise from within the IVar and make it an Async (which is
        // "hot" in that starting it will return "immediately" and be idempotent)
        (prom ^=> id)
        |> Async.Global.ofJob

      else
        async.Return ()

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
        match invocation, defaultName with
        | Log (level, msgFactory) ->
          invocation.ReturnValue <- log level msgFactory

        | LogSimple message ->
          invocation.ReturnValue <- logSimple message

  let create (typ : Type) logger : obj =
    if typ = null then invalidArg "typ" "is null"
    let generator = new ProxyGenerator()
    let facade = I logger :> IInterceptor
    generator.CreateInterfaceProxyWithoutTarget(typ, facade)

  let createString (typ : string) logger : obj =
    create (Type.GetType typ) logger

  let createGeneric<'logger when 'logger : not struct> logger : 'logger =
    create typeof<'logger> logger :?> 'logger