namespace Logary.CSharp

open Logary
open System
open Hopac
open Hopac.Infixes
open System.Threading
open System.Threading.Tasks
open System.Runtime.CompilerServices
open Logary.Model


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


[<Extension>]
type LoggerEx =
  [<Extension>]
  static member Log(logger: Logger, m: LogaryMessageBase, ct: CancellationToken): Task =
    upcast Alt.toTask ct (logger.logBP(m) ^->. ())

  [<Extension>]
  static member LogWithAck(logger: Logger, m: LogaryMessageBase, ct: CancellationToken): Task =
    upcast Alt.toTask ct (
      logger.logWithAck(true, m) ^=> function
        | Ok ack -> ack
        | Result.Error _ -> Promise.unit)
    
