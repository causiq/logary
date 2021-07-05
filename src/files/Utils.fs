#if TYPESHAPE_EXPOSE
module Logary.Internals.TypeShape.Core.Utils
#else
// NB we don't want to leak the `TypeShape` namespace
// to the public API of the assembly
// so we use a top-level internal module
module internal TypeShape_Utils
#endif

open System
open System.Threading
open System.Collections.Generic
open System.Collections.Concurrent
open System.Reflection
open System.Runtime.Serialization
#if TYPESHAPE_EMIT
open System.Reflection.Emit
#endif

type private ICell =
    abstract Type : Type
    abstract IsValueCreated : bool
    abstract Value : obj with get,set

/// Value container that will eventually be populated
type Cell<'T> internal () =
    let mutable isValueCreated = false
    let mutable value = Unchecked.defaultof<'T>
    member __.Id = id
    member __.IsValueCreated : bool = isValueCreated
    member __.Value : 'T =
        if isValueCreated then value else
        sprintf "Value for '%O' has not been initialized." typeof<'T>
        |> invalidOp

    member internal __.Value
        with set t =
            if isValueCreated then
                sprintf "Value for '%O' has already been initialized" typeof<'T>
                |> invalidOp
            value <- t ; isValueCreated <- true

    interface ICell with
        member __.Type = typeof<'T>
        member __.IsValueCreated = __.IsValueCreated
        member __.Value with get () = box __.Value
        member __.Value with set t = __.Value <- unbox t



[<NoEquality; NoComparison>]
type private GenPayload = { Cell : ICell ; DelayedValue : obj ; Dependencies : HashSet<Type> }

[<NoEquality; NoComparison>]
type private CachePayload = { Type : Type ; Value : obj ; Dependencies : HashSet<Type> }

type GenerationToken<'T> = internal | GenerationToken

[<NoEquality; NoComparison>]
type CachedResult<'T> =
    | Cached of value:'T * isValueCreated:bool
    | NotCached of GenerationToken<'T>

/// Helper class for generating recursive values
type TypeGenerationContext internal (parentCache : TypeCache option) =
    let id = let g = Guid.NewGuid() in g.ToString()
    let stack = new Stack<Type>()
    let dict = new Dictionary<Type, GenPayload>()

    let mutable isDisposed = false
    let mutable isAcquired = 0
    let acquire() =
        if isDisposed then raise <| ObjectDisposedException("TypeGenerationContext")
        if Interlocked.CompareExchange(&isAcquired, 1, 0) = 0 then
            { new IDisposable with member __.Dispose() = isAcquired <- 0 }
        else invalidOp "Multi-threaded usage of TypeGenerationContext not supported."

    let registerDependency (child : Type) =
        if stack.Count > 0 then
            let parent = stack.Peek()
            let pcell = dict.[parent]
            let _ = pcell.Dependencies.Add child
            ()

    new () = new TypeGenerationContext(None)

    member internal __.Id = id
    member internal __.ParentCache = parentCache

    /// <summary>
    ///     Registers an uninitialized value for given type or returns
    ///     a cached value if already computed.
    /// </summary>
    /// <param name="delay">Delay function used for defining recursive values.</param>
    member __.InitOrGetCachedValue<'T>(delay : Cell<'T> -> 'T) : CachedResult<'T> =
        use _d = acquire()
        registerDependency typeof<'T>
        let mutable t = Unchecked.defaultof<'T>
        match parentCache with
        | Some c when c.TryGetValue(&t) -> Cached(t, true)
        | _ ->
            let ok,found = dict.TryGetValue typeof<'T>
            if ok then
                if found.Cell.IsValueCreated then
                    Cached(found.Cell.Value :?> 'T, isValueCreated = true)
                else
                    Cached(found.DelayedValue :?> 'T, isValueCreated = false)
            else
                let cell = new Cell<'T>()
                let delayed = delay cell
                let p = { Cell = cell ; DelayedValue = delayed ; Dependencies = HashSet() }
                stack.Push typeof<'T>
                dict.Add(typeof<'T>, p)
                NotCached GenerationToken

    /// Commits computed value to the generator.
    member __.Commit<'T> (_ : GenerationToken<'T>) (value : 'T) : 'T =
        use _d = acquire()
        if stack.Count = 0 || stack.Pop() <> typeof<'T> then invalidOp "TypeGenerationContext: unexpected commit operation"
        let p = dict.[typeof<'T>]
        p.Cell.Value <- value
        value

    member internal __.GetGeneratedValues() =
        use _d = acquire()
        isDisposed <- true
        if stack.Count > 0 then [||] else

        dict
        |> Seq.map (function KeyValue(_,v) -> v.Cell.Type, v.Cell.Value, Seq.toArray v.Dependencies)
        |> Seq.toArray

    interface IDisposable with
        member __.Dispose() =
            match parentCache with
            | Some pc -> pc.Commit __
            | None -> ()

/// Thread-safe cache of values indexed by type.
and TypeCache private (cache : ConcurrentDictionary<Type, CachePayload>) =
    let generators = new ConcurrentDictionary<string, unit>()

    let lockObj = new obj()
    let withLockedCache f =
        lock lockObj (fun () ->
            while generators.Count > 0 do Thread.SpinWait 20
            f ())

    let awaitUnlock() = lock lockObj id

    let clone (cache : ConcurrentDictionary<Type, CachePayload>) =
        cache
        |> Seq.map (fun kv ->
            let payload = kv.Value
            let payload2 = { payload with Dependencies = HashSet(payload.Dependencies) }
            KeyValuePair(kv.Key, payload2))
        |> ConcurrentDictionary

    let rec cleanup (ts : Type list) =
        match ts with
        | [] -> ()
        | t :: rest ->
            let ok, p = cache.TryGetValue t
            if ok then
                let _ = cache.TryRemove t
                cleanup (Seq.toList p.Dependencies @ rest)
            else
                cleanup rest

    new () = TypeCache(new ConcurrentDictionary<_,_>())

    /// Total number of items in cache
    member __.Count = cache.Count
    /// Checks whether the supplied type is contained in cache
    member __.ContainsKey<'T>() = cache.ContainsKey typeof<'T>
    /// Checks whether the supplied type is contained in cache
    member __.ContainsKey(t : Type) = cache.ContainsKey t
    /// Gets all types registered in the cache
    member __.Keys = cache.Keys |> Seq.map id
    /// Gets all values registered in the cache
    member __.Values = cache.Values |> Seq.map (fun p -> p.Value)

    /// Try looking up cached value by type
    member __.TryGetValue<'T>(result : byref<'T>) : bool =
        let mutable p = Unchecked.defaultof<_>
        if cache.TryGetValue(typeof<'T>, &p) then
            result <- p.Value :?> 'T ; true
        else
            false

    /// Try looking up cached value by type
    member __.TryGetValue(t : Type, result : byref<obj>) : bool =
        let mutable p = Unchecked.defaultof<_>
        if cache.TryGetValue(t, &p) then
            result <- p.Value ; true
        else
            false

    /// Try looking up cached value by type
    member __.TryFind<'T>() : 'T option =
        let mutable p = Unchecked.defaultof<_>
        if cache.TryGetValue(typeof<'T>, &p) then Some(p.Value :?> 'T)
        else None

    /// Try looking up cached value by type
    member __.TryFind(t : Type) : obj option =
        let mutable p = Unchecked.defaultof<_>
        if cache.TryGetValue(t, &p) then Some p.Value
        else None

    /// Removes given type and any dependencies from cache
    /// This will clean up any dependencies on that type too.
    member __.Remove(t : Type) =
        fun () -> cleanup [t]
        |> withLockedCache

    /// Forces update for value of given type
    /// This will clean up any dependencies on that type too.
    member __.ForceAdd<'T>(value : 'T) =
        fun () ->
            cleanup [typeof<'T>]
            cache.[typeof<'T>] <- { Type = typeof<'T> ; Value = value ; Dependencies = HashSet() }
        |> withLockedCache

    /// Creates a TypeGenerationContext that is bound to the current cache.
    /// Values generated by the manager can be committed back to the
    /// cache once completed.
    member __.CreateGenerationContext() =
        do awaitUnlock()
        let generator = new TypeGenerationContext(Some __)
        generators.[generator.Id] <- ()
        generator

    /// Commits the generates state by a completed TypeGenerationContext instance.
    member __.Commit(ctx : TypeGenerationContext) =
        if not <| generators.ContainsKey ctx.Id then
            invalidArg "ctx" "TypeGenerationContext does not belong to TypeCache context."

        let values = ctx.GetGeneratedValues()

        for t,v,_ in values do
            let _ = cache.GetOrAdd(t, fun t -> { Type = t ; Value = v ; Dependencies = HashSet() })
            ()

        for t,_,deps in values do
            for d in deps do
                let p = cache.[d]
                let _ = lock p.Dependencies (fun () -> p.Dependencies.Add t)
                ()

        let _ = generators.TryRemove(ctx.Id)
        ()

    /// Creates a clone of the current cache items
    member __.Clone() =
        fun () -> new TypeCache(clone cache)
        |> withLockedCache

//-------------------------------------------------------------

/// Provides a binary search implementation for string values
[<Sealed>]
type BinSearch(inputs : seq<string>) =
    let inputs = inputs |> Seq.map id |> Seq.toArray
    do
        let duplicates =
            inputs
            |> Seq.groupBy id
            |> Seq.filter(fun (_,gp) -> Seq.length gp > 1)
            |> Seq.map fst
            |> Seq.toArray

        if duplicates.Length > 0 then
            duplicates
            |> Seq.map (sprintf "%A")
            |> String.concat ","
            |> sprintf "duplicate values %s found"
            |> invalidArg "inputs"

    let indices, sortedInputs =
        inputs
        |> Seq.mapi (fun i v -> i,v)
        |> Seq.sortBy snd
        |> Seq.toArray
        |> Array.unzip

    /// Gets the original input array used to form
    /// this binary search implementation
    member __.Values = inputs

    /// Returns an integer indicating the position of the
    /// given value in the source array, or -1 if not found.
    member __.TryFindIndex(value : string) : int =
        match sortedInputs.Length with
        | 0 -> -1
        | 1 -> if sortedInputs.[0] = value then 0 else -1
        | n ->
            let mutable found = false
            let mutable lb, ub = 0, n - 1
            let mutable i = 0

            while not found && ub - lb >= 0 do
                i <- (lb + ub) / 2
                match compare value sortedInputs.[i] with
                | 0 -> found <- true
                | c when c < 0 -> ub <- i - 1
                | _ -> lb <- i + 1

            if found then indices.[i] else -1


//--------------------------------------------------------------

type private ShallowObjectCopier<'T> private () =
    static let copier : Lazy<Action<'T, 'T>> = lazy(
        let t = typeof<'T>
        if not t.IsClass then invalidArg t.FullName "unsupported type"

        let rec gatherFields (t:Type) = seq {
            match t.BaseType with
            | null -> ()
            | bt -> yield! gatherFields bt

            let flags = BindingFlags.Instance ||| BindingFlags.Public ||| BindingFlags.NonPublic
            yield! t.GetFields flags
        }

        let fields = gatherFields t |> Seq.toArray

#if TYPESHAPE_EMIT
        let voidType = Type.GetType "System.Void"
        let dynamicMethod =
            new DynamicMethod("shallowCopy", MethodAttributes.Static ||| MethodAttributes.Public,
                                CallingConventions.Standard, voidType, [|t;t|], typeof<ShallowObjectCopier<'T>>,
                                skipVisibility = true)

        let ilGen = dynamicMethod.GetILGenerator()
        for f in fields do
            ilGen.Emit OpCodes.Ldarg_1
            ilGen.Emit OpCodes.Ldarg_0
            ilGen.Emit(OpCodes.Ldfld, f)
            ilGen.Emit(OpCodes.Stfld, f)

        ilGen.Emit OpCodes.Ret
        dynamicMethod.CreateDelegate(typeof<Action<'T,'T>>) :?> Action<'T,'T>
#else
        new Action<'T,'T>(fun src dst ->
            for f in fields do
                let v = f.GetValue(src)
                f.SetValue(dst, v))
#endif
        )

    static member Copy (source : 'T) (target : 'T) = copier.Value.Invoke(source, target)

/// Helper methods used for constructing cyclic values
type RecursiveValueHelper =
    /// Creates an uninitialized value for given type
    static member CreateUninitializedValue<'T>() : 'T =
        System.Runtime.Serialization.FormatterServices.GetUninitializedObject(typeof<'T>) :?> 'T

    /// performs a shallow copy of field contents from one object to another
    static member ShallowCopy<'T when 'T : not struct>(source : 'T) (target : 'T) =
        ShallowObjectCopier.Copy source target

/// Helper class for detecting cycles in a traversed object graph
[<Sealed>]
type ObjectStack() =
    let mutable idGen = new ObjectIDGenerator()
    let objStack = new Stack<int64>()

    let mutable firstTime = true
    let mutable isCycle = false
    let mutable objId = -1L

    /// Resets state for given stack instance
    member __.Reset() =
        idGen <- new ObjectIDGenerator()
        objStack.Clear()
        firstTime <- true
        isCycle <- false
        objId <- -1L

    member __.Depth = objStack.Count

    /// Id of the last object pushed to the stack
    member __.ObjectId = objId
    /// Indicates whether the last object was pushed to the stack for the first time
    member __.IsFirstTime = firstTime
    /// Indicates whether the current value already exists in the stack
    member __.IsCycle = isCycle

    member __.Push<'T when 'T : not struct>(t : 'T) : unit =
        let id = idGen.GetId(t, &firstTime)
        isCycle <- objStack.Contains id
        objStack.Push id
        objId <- id

    member __.Pop() : unit =
        let _ = objStack.Pop()
        objId <- -1L
        isCycle <- false
        firstTime <- true

/// Helper class for re-constructing cyclic object graphs
[<Sealed>]
type ObjectCache() =
    let dict = new Dictionary<int64, obj> ()
    let cyclicValues = new HashSet<int64>()
    /// Returns true if cache has value for given id
    member __.HasValue(id:int64) = dict.ContainsKey id
    /// Gets cached value of given id
    member __.GetValue<'T when 'T : not struct>(id:int64) = dict.[id] :?> 'T
    /// Attempts to get cached value of given id
    member __.TryGetValue<'T when 'T : not struct>(id:int64, value:byref<'T>) : bool =
        let mutable r = null
        if dict.TryGetValue(id, &r) then value <- r :?> 'T ; true
        else false

    /// Adds given value to cache or fixes up existing uninitialized object
    /// by performing a shallow copy on its fields.
    member __.AddValue<'T when 'T : not struct>(id:int64, value:'T) : 'T =
        if cyclicValues.Contains id then
            let target = dict.[id] :?> 'T
            ShallowObjectCopier.Copy value target
            let _ = cyclicValues.Remove id
            target
        else
            dict.[id] <- value
            value

    /// Creates an uninitialized value for given id and
    /// specified concrete type and appends it to the cache.
    member __.CreateUninitializedInstance<'T when 'T : not struct>(id:int64, underlyingType : Type) : 'T =
        let t = System.Runtime.Serialization.FormatterServices.GetUninitializedObject(underlyingType) :?> 'T
        dict.Add(id, t) ; cyclicValues.Add id |> ignore
        t

    /// Resets state for cache instance
    member __.Reset() =
        dict.Clear() ; cyclicValues.Clear()