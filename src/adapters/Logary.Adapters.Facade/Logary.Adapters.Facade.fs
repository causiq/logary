namespace Logary.Adapters.Facade

open Logary
open Logary.Internals
open System
open System.Reflection
open System.Collections.Concurrent
open Castle.DynamicProxy
open Hopac
open Hopac.Extensions
open Hopac.Infixes

module LogaryFacadeAdapter =
  let private findMethod : Type * string -> MethodInfo =
    Cache.memoize (fun (typ, meth) -> typ.GetMethod meth)

  let private findProperty : Type * string -> PropertyInfo =
    Cache.memoize (fun (typ, prop) -> typ.GetProperty prop)

  let toPointValue (o : obj) : PointValue =
    // TODO: remaining work
    Event "hardcoded"

  let toLogLevel (o : obj) : LogLevel =
    let typ = o.GetType()
    let par = typ, "toInt"
    let toInt = findMethod (typ, "toInt")
    LogLevel.ofInt (toInt.Invoke(o, null) :?> int)

  let toMsg (o : obj) : Message =
    let typ = o.GetType()
    let readProperty name = (findProperty (typ, name)).GetValue o

    { name      = PointName (readProperty "name" :?> string [])
      value     = readProperty "value" |> toPointValue
      fields    = Map.empty
      context   = Map.empty
      timestamp = readProperty "timestamp" :?> EpochNanoSeconds
      level     = readProperty "level" |> toLogLevel }
    |> Message.setFieldsFromMap (readProperty "fields" :?> Map<string, obj>)

  let toMsgFactory (o : obj) : unit -> Message =
    let invokeMethod = o.GetType().GetMethod("Invoke")
    fun () -> toMsg (invokeMethod.Invoke(o, [| () |]))

  let (|Log|LogSimple|) (invocation : IInvocation) : Choice<_, _> =
    match invocation.Method.Name with
    | "log" ->
      let level = toLogLevel invocation.Arguments.[0]
      let factory = toMsgFactory invocation.Arguments.[1]
      Log (level, factory)

    | "logSimple" ->
      let msg = toMsg invocation.Arguments.[0]
      LogSimple msg

    | meth ->
      failwithf "Method '%s' should not exist on Logary.Facade.Logger" meth

  /// This is the main adapter which logs from an arbitrary logary facade
  /// into logary. Provide the namespace you put the facade in and the assembly
  /// which it should be loaded from, and this adapter will use (memoized) reflection
  /// to properly bind to the facade.
  type private I(logger : Logger) =

    // Codomains of these two functions are equal to codomains of Facade's
    // functions:

    let log (level : LogLevel) (msgFactory : unit -> Message) : Async<unit> =
      if logger.level <= level then
        let prom = IVar ()

        // kick off the logging no matter what
        start (Logger.logWithAck logger (msgFactory ()) ^=> IVar.fill prom)

        // take the promise from within the IVar and make it an Async (which is
        // "hot" in that starting it will return "immediately" and be idempotent)
        (prom ^=> id) |> Async.Global.ofJob

      else
        async.Return ()

    let logSimple (msg : Message) : unit =
      logger.logSimple msg

    interface IInterceptor with
      member x.Intercept invokation =
        printfn "Invokation"
        printfn "=========="
        printfn "Args: "
        for arg in invokation.Arguments do
          let argType = arg.GetType()
          printfn " - %A\n   : %s" arg (argType.FullName)
          printfn "   :> %s" argType.BaseType.FullName

        printfn "Method.Name: %s" invokation.Method.Name

        match invokation with
        | Log (level, msgFactory) ->
          invokation.ReturnValue <- log level msgFactory

        | LogSimple message ->
          invokation.ReturnValue <- logSimple message

  let create (typ : Type) logger : obj =
    if typ = null then invalidArg "typ" "is null"
    let generator = new ProxyGenerator()
    let facade = I logger :> IInterceptor
    generator.CreateInterfaceProxyWithoutTarget(typ, facade)

  let createString (typ : string) logger : obj =
    create (Type.GetType typ) logger

  let createGeneric<'logger when 'logger : not struct> logger : 'logger =
    create typeof<'logger> logger :?> 'logger