namespace Logary.CSharp

open Logary
open Logary.Message
open System
open Hopac
open System.Threading
open System.Threading.Tasks
open System.Runtime.CompilerServices
open System.Runtime.InteropServices


// This file is partially from
// https://github.com/fsprojects/FSharpx.Extras/blob/master/src/FSharpx.Extras/CSharpCompat.fs

/// Helps the C# compiler with Func type inference.
type L =
  /// Helps the C# compiler with Func type inference.
  static member F (f: Func<_>) = f
  /// Helps the C# compiler with Func type inference.
  static member F (f: Func<_,_>) = f
  /// Helps the C# compiler with Func type inference.
  static member F (f: Func<_,_,_>) = f

/// <summary>
/// Conversion functions from Action/Func to FSharpFunc
/// We need these because FuncConvert often makes C# type inference fail.
/// </summary>
[<Extension>]
type FSharpFunc =
  /// Convert an Action into an F# function returning unit
  static member OfAction (f: Action) =
    fun () -> f.Invoke()

  /// Convert an Action into an F# function returning unit
  static member OfAction (f: Action<_>) =
    fun x -> f.Invoke x

  /// Convert an Action into an F# function returning unit
  static member OfAction (f: Action<_,_>) =
    fun x y -> f.Invoke(x,y)

  /// Convert an Action into an F# function returning unit
  static member OfAction (f: Action<_,_,_>) =
    fun x y z -> f.Invoke(x,y,z)

  /// Convert a Func into an F# function
  static member OfFunc (f: Func<_>) =
    fun () -> f.Invoke()

  /// Convert a Func into an F# function
  static member OfFunc (f: Func<_,_>) =
    fun x -> f.Invoke x

  /// Convert a Func into an F# function
  static member OfFunc (f: Func<_,_,_>) =
    fun x y -> f.Invoke(x, y)

  /// Convert a Func into an F# function
  static member OfFunc (f: Func<_,_,_,_>) =
    fun x y z -> f.Invoke(x, y, z)

  /// Convert a Func into an F# function
  static member OfFunc (f: Func<_,_,_,_, _>) =
    fun x y z a -> f.Invoke(x, y, z, a)

  /// Convert a Func into an F# function
  static member OfFunc (f: Func<_,_,_,_,_,_>) =
    fun x y z a b -> f.Invoke(x, y, z, a, b)

  /// Convert a Func into an F# function
  static member OfFunc (f: Func<_,_,_,_,_,_,_>) =
    fun x y z a b c -> f.Invoke(x, y, z, a, b, c)

/// Extensions around Actions and Funcs
[<Extension>]
type Funcs =
  /// Converts an action to a function returning Unit
  [<Extension>]
  static member ToFunc (a: Action) =
    Func<_>(a.Invoke)

  /// Converts an action to a function returning Unit
  [<Extension>]
  static member ToFunc (a: Action<_>) =
    Func<_,_>(a.Invoke)

  /// Converts an action to a function returning Unit
  [<Extension>]
  static member ToFunc (a: Action<_,_>) =
    Func<_,_,_>(curry a.Invoke)

  /// Converts an action to a function returning Unit
  [<Extension>]
  static member ToFunc (f: Action<_,_,_>) =
    Func<_,_,_,_>(fun a b c -> f.Invoke(a, b, c))

  /// Converts an action to a function returning Unit
  [<Extension>]
  static member ToFunc (f: Action<_,_,_, _>) =
    Func<_,_,_,_, _>(fun a b c d -> f.Invoke(a, b, c, d))

  /// Converts an action to a function returning Unit
  [<Extension>]
  static member ToFunc (f: Action<_,_,_, _, _>) =
    Func<_,_,_,_,_,_>(fun a b c d e -> f.Invoke(a, b, c, d, e))

  /// Converts an action to a function returning Unit
  [<Extension>]
  static member ToFunc (g: Action<_,_,_, _, _, _>) =
    Func<_,_,_,_, _, _, _>(fun a b c d e f -> g.Invoke(a, b, c, d, e, f))

  /// Convert a F# function to a CLR Func
  [<Extension>]
  static member ToFunc (f: unit -> _) =
    Func<_> f

  /// Convert a F# function to a CLR Func
  [<Extension>]
  static member ToFunc (f: _ -> _) =
    Func<_, _> f

  /// Convert a F# function to a CLR Func
  [<Extension>]
  static member ToFunc (f: _ -> _ -> _) =
    Func<_, _, _> f

  /// Convert a F# function to a CLR Func
  [<Extension>]
  static member ToFunc (f: _ -> _ -> _ -> _) =
    Func<_, _, _, _> f

  /// Convert a F# function to a CLR Func
  [<Extension>]
  static member ToFunc (f: _ -> _ -> _ -> _ -> _) =
    Func<_, _, _, _, _> f

  /// Convert a F# function to a CLR Func
  [<Extension>]
  static member ToFunc (f: _ -> _ -> _ -> _ -> _ -> _) =
    Func<_, _, _, _, _, _> f

  /// Convert a F# function to a CLR Func
  [<Extension>]
  static member ToFunc (f: _ -> _ -> _ -> _ -> _ -> _ -> _) =
    Func<_, _, _, _, _, _, _> f

  /// Convert a Func into an F# function
  [<Extension>]
  static member ToFSharpFunc (f: Func<_>): unit -> _ =
    f.Invoke

  /// Convert a Func into an F# function
  [<Extension>]
  static member ToFSharpFunc (f: Func<_, _>) =
    fun x -> f.Invoke x

  /// Convert a Func into an F# function
  [<Extension>]
  static member ToFSharpFunc (f: Func<_, _, _>) =
    fun x y -> f.Invoke(x, y)

  /// Convert a Func into an F# function
  [<Extension>]
  static member ToFSharpFunc (f: Func<_, _, _, _>) =
    fun x y z -> f.Invoke(x, y, z)

  /// Convert a Func into an F# function
  [<Extension>]
  static member ToFSharpFunc (f: Func<_, _, _, _, _>) =
    fun x y z a -> f.Invoke(x, y, z, a)

  /// Convert a Func into an F# function
  [<Extension>]
  static member ToFSharpFunc (f: Func<_, _, _, _, _, _>) =
    fun x y z a b -> f.Invoke(x, y, z, a, b)

  /// Convert a Func into an F# function
  [<Extension>]
  static member ToFSharpFunc (f: Func<_, _, _, _, _, _, _>) =
    fun x y z a b c -> f.Invoke(x, y, z, a, b, c)

  /// Converts an uncurried function to a curried function
  [<Extension>]
  static member Curry (f: Func<_,_,_>) =
    Func<_,Func<_,_>>(fun a -> Func<_,_>(fun b -> f.Invoke(a,b)))

  /// Converts an uncurried function to a curried function
  [<Extension>]
  static member Curry (f: Func<_,_,_,_>) =
    Func<_,Func<_,Func<_,_>>>(fun a -> Func<_,Func<_,_>>(fun b -> Func<_,_>(fun c -> f.Invoke(a,b,c))))

  /// Converts an action with 2 arguments into an action taking a 2-tuple
  [<Extension>]
  static member Tuple (f: Action<_,_>) =
    Action<_>(fun (a,b) -> f.Invoke(a,b))

  /// Converts an action with 3 arguments into an action taking a 3-tuple
  [<Extension>]
  static member Tuple (f: Action<_,_,_>) =
    Action<_>(fun (a,b,c) -> f.Invoke(a,b,c))

  /// Converts an action with 4 arguments into an action taking a 4-tuple
  [<Extension>]
  static member Tuple (f: Action<_,_,_,_>) =
    Action<_>(fun (a,b,c,d) -> f.Invoke(a,b,c,d))

  /// Converts an action with 5 arguments into an action taking a 5-tuple
  [<Extension>]
  static member Tuple (g: Action<_,_,_,_,_>) =
    Action<_>(fun (a,b,c,d,e) -> g.Invoke(a,b,c,d,e))

  /// Converts an action with 6 arguments into an action taking a 6-tuple
  [<Extension>]
  static member Tuple (g: Action<_,_,_,_,_,_>) =
    Action<_>(fun (a,b,c,d,e,f) -> g.Invoke(a,b,c,d,e,f))

  /// Converts an action taking a 2-tuple into an action with 2 parameters
  [<Extension>]
  static member Untuple (f: Action<_ * _>) =
    Action<_,_>(fun a b -> f.Invoke(a,b))

  /// /// Converts an action taking a 3-tuple into an action with 3 parameters
  [<Extension>]
  static member Untuple (f: Action<_ * _ * _>) =
    Action<_,_,_>(fun a b c -> f.Invoke(a,b,c))

  /// Converts an action taking a 4-tuple into an action with 4 parameters
  [<Extension>]
  static member Untuple (f: Action<_ * _ * _ * _>) =
    Action<_,_,_,_>(fun a b c d -> f.Invoke(a,b,c,d))

  /// Converts an action taking a 5-tuple into an action with 5 parameters
  [<Extension>]
  static member Untuple (g: Action<_ * _ * _ * _ * _>) =
    Action<_,_,_,_,_>(fun a b c d e -> g.Invoke(a,b,c,d,e))

  /// Converts an action taking a 6-tuple into an action with 6 parameters
  [<Extension>]
  static member Untuple (g: Action<_ * _ * _ * _ * _ * _>) =
    Action<_,_,_,_,_,_>(fun a b c d e f -> g.Invoke(a,b,c,d,e,f))

  /// Composes two functions.
  /// Mathematically: f . g
  [<Extension>]
  static member Compose (f: Func<_,_>, g: Func<_,_>) =
    Func<_,_>(fun x -> f.Invoke(g.Invoke(x)))

  /// Composes two functions (forward composition).
  /// Mathematically: g . f
  [<Extension>]
  static member AndThen (f: Func<_,_>, g: Func<_,_>) =
    Func<_,_>(fun x -> g.Invoke(f.Invoke(x)))

  /// Extension method on a func that times the executing of the function.
  [<Extension>]
  static member Time (a: Action<'input>, [<ParamArray>] pointName: string[]): Func<'input, Message> =
    let timef = Message.time (PointName pointName) (FSharpFunc.OfAction a)
    Funcs.ToFunc (timef >> snd)

  /// Extension method on a func that times the executing of the function.
  [<Extension>]
  static member Time (a: Action<'a, 'b>, [<ParamArray>] pointName: string[]): Func<'a, 'b, Message> =
    let timef =
      FSharpFunc.OfAction a
      >> Message.time (PointName pointName)
      >> fun resf -> resf >> snd
    Funcs.ToFunc<_, _, _> timef

  /// Extension method on a func that times the executing of the function.
  [<Extension>]
  static member Time (a: Action<'a, 'b, 'c>, [<ParamArray>] pointName: string[]): Func<'a, 'b, 'c, Message> =
    let timef =
      FSharpFunc.OfAction a
      >> uncurry
      >> Message.time (PointName pointName)
      >> fun resf -> resf >> snd
      >> curry
    Funcs.ToFunc<_, _, _, _> timef

  /// Extension method on a func that times the executing of the function.
  [<Extension>]
  static member Time (f: Func<'input,'output>, [<ParamArray>] pointName: string[]): Func<'input, 'output * Message> =
    let timef = Message.time (PointName pointName) (Funcs.ToFSharpFunc f)
    Funcs.ToFunc timef

  /// Extension method on a func that times the executing of the function.
  [<Extension>]
  static member Time (f: Func<'input, _, 'output>, [<ParamArray>] pointName: string[]): Func<'input, _, 'output * Message> =
    let timef = Funcs.ToFSharpFunc f >> Message.time (PointName pointName)
    Funcs.ToFunc<_, _, _> timef

  /// Extension method on a func that times the executing of the function.
  [<Extension>]
  static member Time (f: Func<'input, _, _, 'output>, [<ParamArray>] pointName: string[]): Func<'input, _, _, 'output * Message> =
    let timef =
      Funcs.ToFSharpFunc f
      >> uncurry
      >> Message.time (PointName pointName)
      >> curry
    Funcs.ToFunc<_, _, _, _> timef

[<Extension>]
type FSharpOption =
  [<Extension>]
  static member HasValue o =
    Option.isSome o

  [<Extension>]
  static member ToNullable o =
    match o with
    | Some x ->
      Nullable x
    | _ ->
      Nullable()

  [<Extension>]
  static member ToFSharpOption (n: Nullable<_>) =
    if n.HasValue then
      Some n.Value
    else
      None

  [<Extension>]
  static member ToFSharpOption v =
    match box v with
    | null ->
      None
    | :? DBNull ->
      None
    | _ ->
      Some v

  static member Some a =
    Option.Some a

  [<Extension>]
  static member Match (o, ifSome: Func<_,_>, ifNone: Func<_>) =
    match o with
    | Some x ->
      ifSome.Invoke x
    | _ ->
      ifNone.Invoke()

  [<Extension>]
  static member Match (o, ifSome: Func<_,_>, ifNone) =
    match o with
    | Some x ->
      ifSome.Invoke x
    | _ ->
      ifNone

  [<Extension>]
  static member Match (o, ifSome: Action<_>, ifNone: Action) =
    match o with
    | Some x ->
      ifSome.Invoke x
    | _ ->
      ifNone.Invoke()

  [<Extension>]
  static member Do (o, f: Action<_>) =
    match o with
    | Some v ->
      f.Invoke v
    | _ ->
      ()

  /// Gets the option if Some x, otherwise the supplied default value.
  [<Extension>]
  static member OrElse (o, other) =
    match o with
    | Some x ->
      Some x
    | _ ->
      other

  [<Extension>]
  static member GetOrElse (o, other) =
    match o with
    | Some x ->
      x
    | _ ->
      other

  [<Extension>]
  static member GetOrElse (o, other: _ Func) =
    match o with
    | Some x ->
      x
    | _ ->
      other.Invoke()

  [<Extension>]
  static member GetOrDefault (o: Option<_>) =
    match o with
    | Some x ->
      x
    | _ ->
      Unchecked.defaultof<_>

  [<Extension>]
  static member ToFSharpChoice (o, other) =
    match o with
    | Some v ->
      Choice1Of2 v
    | _ ->
      Choice2Of2 other

  /// Converts the option to a list of length 0 or 1
  [<Extension>]
  static member ToFSharpList o =
    Option.toList o

  /// Converts the option to an array of length 0 or 1
  [<Extension>]
  static member ToArray o =
    Option.toArray o

  /// Transforms an option value by using a specified mapping function
  [<Extension>]
  static member Select (o, f: Func<_,_>) =
    Option.map f.Invoke o

  /// Invokes a function on an optional value that itself yields an option
  [<Extension>]
  static member SelectMany (o, f: Func<_,_>) =
    Option.bind f.Invoke o

  /// Invokes a function on an optional value that itself yields an option,
  /// and then applies a mapping function
  [<Extension>]
  static member SelectMany (o, f: Func<_,_>, mapper: Func<_,_,_>) =
    let mapper = Option.lift2 (curry mapper.Invoke)
    let v = Option.bind f.Invoke o
    mapper o v

  /// <summary>
  /// Evaluates the equivalent of <see cref="System.Linq.Enumerable.Aggregate"/> for an option
  /// </summary>
  [<Extension>]
  static member Aggregate (o, state, f: Func<_,_,_>) =
    Option.fold (curry f.Invoke) state o

  /// Applies a predicate to the option. If the predicate returns true, returns Some x, otherwise None.
  [<Extension>]
  static member Where (o: _ option, pred: _ Predicate) =
    Option.filter pred.Invoke o

  static member SomeUnit =
    Some()

[<Extension>]
type FSharpList =
  [<Extension>]
  static member Match (l, empty: Func<_>, nonempty: Func<_,_,_>) =
    match l with
    | [] -> empty.Invoke()
    | x::xs -> nonempty.Invoke(x,xs)

  [<Extension>]
  static member Choose (l, chooser: Func<_,_>) =
    List.choose chooser.Invoke l

  [<Extension>]
  static member TryFind (l, pred: _ Predicate) =
    List.tryFind pred.Invoke l

  [<Extension>]
  static member TryFind (l, value) =
    List.tryFind ((=) value) l

  [<Extension>]
  static member Cons (l, e) =
    e::l

  static member Create([<ParamArray>] values: 'T1[]) =
    Seq.toList values

  [<Extension>]
  static member ToFSharpList s =
    Seq.toList s

[<Extension>]
type FSharpSet =
  static member Create([<ParamArray>] values: 'T1[]) =
    set values

  [<Extension>]
  static member ToFSharpSet values =
    set values

[<Extension>]
type FSharpMap =
  static member Create([<ParamArray>] values) =
    Map.ofArray values

  [<Extension>]
  static member ToFSharpMap values =
    Map.ofSeq values

module private MiscHelpers =
  let private disposable =
    { new IDisposable with
        member x.Dispose() = () }

  let inline subscribe (ct: CancellationToken) (f: unit -> unit) =
    match ct with
    | _ when ct = CancellationToken.None ->
      disposable
    | ct ->
      upcast ct.Register (Action f)

  let inline chooseLogFun logger logLevel backpressure flush timeoutMillis =
    if flush then
      Logger.logWithAck logger logLevel
      >> Alt.afterJob id // Alt<Promise<unit>> -> Promise<unit>
      >> Alt.afterFun (fun () -> true) // Promise<unit> -> bool
    elif backpressure then
      Logger.log logger logLevel >> Alt.afterFun (fun _ -> true)
    else
      Logger.logWithTimeout logger timeoutMillis logLevel

/// Functions callable by Logary.CSharp.
[<Extension>]
type Job =
  [<Extension>]
  static member ToTask<'a> (xJ: Job<'a>): Task<'a> =
    let tcs = new TaskCompletionSource<'a>()
    xJ |> Job.map (tcs.SetResult >> ignore) |> start
    tcs.Task

module Alt =

  let toTask (ct: CancellationToken) (xA: Alt<'res>): Task<'res> =
    // attach to parent because placing in buffer should be quick in the normal case
    let tcs = TaskCompletionSource<'res>(TaskCreationOptions.AttachedToParent) // alloc TCS
    let nack = IVar () // alloc
    let sub = MiscHelpers.subscribe ct (fun () -> start (IVar.fill nack ())) // only alloc IVar if ct <> None
    start (
      Alt.tryFinallyFun (
        Alt.choose [
          Alt.tryIn xA
                    (fun res -> Job.thunk (fun () -> tcs.SetResult res))
                    (fun ex -> Job.thunk (fun () -> tcs.SetException ex))
          nack |> Alt.afterFun tcs.SetCanceled // |> Alt.afterFun (fun () -> printfn "Cancelled")
        ]
      ) sub.Dispose
    )
    // alloc on access?
    tcs.Task

  let internal toTasks bufferCt promiseCt (xAP: Alt<Promise<unit>>): Task<Task> =
    xAP
    |> Alt.afterFun (fun prom -> toTask promiseCt prom :> Task)
    |> toTask bufferCt

[<Extension>]
type LoggerExtensions =

  // corresponds to: log, logWithTimeout

  /// Log a message, but don't await all targets to flush. With NO back-pressure by default.
  /// Backpressure implies the caller will wait until its message is in the buffer.
  /// The timeout-milliseconds parameter is only used if backpressure is false.
  /// If not using backpressure, the returned task yields after either 5 seconds or
  /// when the message is accepted to all targets' ring buffers.
  /// Flush implies backpressure.
  ///
  /// Remember to use the transform function to add an event template.
  [<Extension>]
  static member Log (logger: Logger,
                     logLevel: LogLevel,
                     transform: Func<Message, Message>,
                     [<Optional; DefaultParameterValue(true)>] backpressure: bool,
                     [<Optional; DefaultParameterValue(true)>] flush: bool,
                     [<Optional; DefaultParameterValue(5000u)>] timeoutMillis: uint32)
                    : Task =
    let timeoutMillis = if timeoutMillis = 0u then 5000u else timeoutMillis
    let logFn = MiscHelpers.chooseLogFun logger logLevel backpressure flush timeoutMillis
    let transform = Funcs.ToFSharpFunc transform
    upcast Alt.toTask CancellationToken.None (eventX "EVENT" >> transform |> logFn)

  // corresponds to: log, logWithTimeout, logWithAck

  /// Log an event, but don't await all targets to flush. With NO back-pressure by default.
  /// Backpressure implies the caller will wait until its message is in the buffer.
  /// The timeout-milliseconds parameter is only used if backpressure is false.
  /// If not using backpressure, the returned task yields after either 5 seconds or
  /// when the message is accepted to all targets' ring buffers.
  /// Flush implies backpressure.
  [<Extension>]
  static member LogEvent(logger: Logger,
                         level: LogLevel,
                         formatTemplate: string,
                         [<Optional; DefaultParameterValue(null:obj)>] fieldsObj: obj,
                         [<Optional; DefaultParameterValue(null:Exception)>] exn: Exception,
                         [<Optional; DefaultParameterValue(null:Func<Message,Message>)>] transform: Func<Message, Message>,
                         [<Optional; DefaultParameterValue(true)>] backpressure: bool,
                         [<Optional; DefaultParameterValue(true)>] flush: bool,
                         [<Optional; DefaultParameterValue(5000u)>] timeoutMillis: uint32)
                        : Task =
    let timeoutMillis = if timeoutMillis = 0u then 5000u else timeoutMillis
    let transform = if isNull transform then id else FSharpFunc.OfFunc transform
    let fields = if isNull fieldsObj then obj() else fieldsObj
    let logFn = MiscHelpers.chooseLogFun logger level backpressure flush timeoutMillis

    let messageFactory =
      eventX formatTemplate
      >> setFieldsFromObject fields
      >> if isNull exn then id else Message.addExn exn
      >> transform

    upcast Alt.toTask CancellationToken.None (logFn messageFactory)




  /// Log an event, but don't await all targets to flush. WITH back-pressure by default.
  /// Backpressure implies the caller will wait until its message is in the buffer.
  [<Extension>]
  static member LogEventFormat(logger: Logger,
                               level: LogLevel,
                               formatTemplate: string,
                               [<ParamArray>] args: obj[])
                              : Task =
    let msgFac = fun _ -> Message.eventFormat (level, formatTemplate, args)
    let call = Logger.log logger level msgFac |> Alt.afterFun (fun _ -> true)
    upcast Alt.toTask CancellationToken.None call

  /// Log a gauge, but don't await all targets to flush. With NO back-pressure by default.
  /// Backpressure implies the caller will wait until its message is in the buffer.
  /// The timeout-milliseconds parameter is only used if backpressure is false.
  /// If not using backpressure, the returned task yields after either 5 seconds or
  /// when the message is accepted to all targets' ring buffers.
  /// Flush implies backpressure.
  /// Returns true of the log was accepted to Logary.
  [<Extension>]
  static member LogGauge(logger: Logger,
                         value: float,
                         units: Units,
                         gaugeName: string,
                         [<Optional; DefaultParameterValue(null:obj)>] fields: obj,
                         [<Optional; DefaultParameterValue(null:Func<Message, Message>)>] transform: Func<Message, Message>,
                         [<Optional; DefaultParameterValue(true)>] backpressure: bool,
                         [<Optional; DefaultParameterValue(true)>] flush: bool,
                         [<Optional; DefaultParameterValue(5000u)>] timeoutMillis: uint32)
                        : Task<bool> =
    let timeoutMillis = if timeoutMillis = 0u then 5000u else timeoutMillis
    let transform = if isNull transform then id else FSharpFunc.OfFunc transform
    let fields = if isNull fields then obj() else fields

    let message =
      gaugeWithUnit logger.name gaugeName (Gauge (Float value, units))
      |> setFieldsFromObject fields

    let logFn = MiscHelpers.chooseLogFun logger message.level backpressure flush timeoutMillis

    Alt.toTask CancellationToken.None (logFn (fun _ -> message))

  // corresponds to: logSimple

  /// Log a message, but don't synchronously wait for the message to be placed
  /// inside Logary's buffers. Instead the message will be added to Logary's
  /// buffers asynchronously with a timeout of 5 seconds, and will then be
  /// dropped. We avoid the unbounded buffer problem by dropping the message.
  /// If you have dropped messages, they will be logged to STDERR. You should load-
  /// test your app to ensure that your targets can send at a rate high enough
  /// without dropping messages.
  [<Extension>]
  static member LogSimple (logger, message): unit =
    Logger.logSimple logger message

  /// Log a message, which returns an inner Task. The outer Task denotes having the
  /// Message placed in all Targets' buffers. The inner Task denotes having
  /// the message properly flushed to all targets' underlying "storage". Targets
  /// whose rules do not match the message will not be awaited. The cancellation token
  /// can cancel the outer Task, but once it's in the buffers, the Log Message cannot
  /// be retracted by cancelling; however if you await the inner task (the promise) then
  /// the `promiseCt` allows you to cancel that await.
  [<Extension>]
  static member LogWithAck (logger, level, transform: Func<Message, Message>, bufferCt, promiseCt): Task<Task> =
    let messageFactory =
      eventX "EVENT"
      >> Funcs.ToFSharpFunc transform
    Alt.toTasks bufferCt promiseCt (Logger.logWithAck logger level messageFactory)

  /// Log a message, which returns an inner Task. The outer Task denotes having the
  /// Message placed in all Targets' buffers. The inner Task denotes having
  /// the message properly flushed to all targets' underlying "storage". Targets
  /// whose rules do not match the message will not be awaited.
  [<Extension>]
  static member LogWithAck (logger: Logger, message: Message): Task<Task> =
    let bufferCt = Unchecked.defaultof<CancellationToken>
    let promiseCt = Unchecked.defaultof<CancellationToken>
    Alt.toTasks bufferCt promiseCt (Logger.logWithAck logger message.level (fun _ -> message))

  // TODO: timeAsyncWithAck, timeAsyncSimple
  // TODO: timeJobWithAck, timeJobSimple
  // TODO: timeAltWithAck, timeAltSimple

  /// NOTE: You need to execute the returned action. Logary.CSharp.MessageExtensions.TimeScope.
  ///
  /// Run the function `action` and measure how long it takes; logging that
  /// measurement as a Gauge in the unit Scaled(Seconds, 10^9). Finally transform the
  /// message using the `transform` function, if given.
  ///
  /// Logs, but doesn't await all targets to flush. Without back-pressure (5 s timeout instead).
  /// The call you make to the function yields directly (after your code is done executing of course).
  [<Extension>]
  static member Time (logger: Logger,
                      action: Action,
                      [<Optional; DefaultParameterValue(null:string)>] measurement: string,
                      [<Optional; DefaultParameterValue(null:Func<Message, Message>)>] transform: Func<Message, Message>)
                     : Action =
    let action = FSharpFunc.OfAction action
    let transform = if isNull transform then id else FSharpFunc.OfFunc transform
    let runnable = logger.timeFun (action, measurement, transform)
    //Action runnable
    failwith "TODO"


  /// Create a new scope that starts a stopwatch on creation and logs the gauge of
  /// the duration its lifetime.
  [<Extension>]
  static member TimeScope (logger: Logger,
                           [<Optional; DefaultParameterValue(null:string)>] nameEnding: string,
                           [<Optional; DefaultParameterValue(null:Func<Message,Message>)>] transform: Func<Message, Message>)
                          : TimeScope =
    let transform = if isNull transform then id else FSharpFunc.OfFunc transform
    logger.timeScopeT nameEnding transform