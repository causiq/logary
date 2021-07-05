module Logary.Internals.Aether

open System

//  Optics

/// Lens from 'a -> 'b.
type Lens<'a,'b> =
    ('a -> 'b) * ('b -> 'a -> 'a)

/// Prism from 'a -> 'b.
type Prism<'a,'b> =
    ('a -> 'b option) * ('b -> 'a -> 'a)

//  Morphisms

/// Isomorphism between 'a <> 'b.
type Isomorphism<'a,'b> =
    ('a -> 'b) * ('b -> 'a)

/// Epimorphism between 'a <> 'b.
type Epimorphism<'a,'b> =
    ('a -> 'b option) * ('b -> 'a)

(* Obsolete

    Backwards compatibility shims to make the 2.x-> 3.x transition
    less painful, providing functionally equivalent options where possible.

    To be removed for 9.x releases. *)

/// Partial lens from a -> b
[<Obsolete ("Use Prism<'a, 'b> instead.")>]
type PLens<'a,'b> = Prism<'a, 'b>

/// Total isomorphism of a <> b
[<Obsolete ("Use Isomorphism<'a, 'b> instead.")>]
type Iso<'a,'b> = Isomorphism<'a, 'b>

/// Partial isomorphism of a <> b
[<Obsolete ("Use Epimorphism<'a, 'b> instead.")>]
type PIso<'a,'b> = Epimorphism<'a, 'b>

/// Functions for composing lenses and prisms with other optics, which
/// returns a new lens or prism based on the optic composed. Open `Aether.Operators`
/// to use the infix operator forms of these compositions, which is significantly
/// less verbose.
[<RequireQualifiedAccess>]
module Compose =

    /// Static overloads of the composition function for lenses (>->).
    /// These functions do not generally need to be called directly, but will
    /// be used when calling Compose.optic.
    type Lens =
        | Lens with

        static member (>->) (Lens, (g2, s2): Lens<'b,'c>) =
            fun ((g1, s1): Lens<'a,'b>) ->
                (fun a -> g2 (g1 a)),
                (fun c a -> s1 (s2 c (g1 a)) a) : Lens<'a,'c>

        static member (>->) (Lens, (g2, s2): Prism<'b,'c>) =
            fun ((g1, s1): Lens<'a,'b>) ->
                (fun a -> g2 (g1 a)),
                (fun c a -> s1 (s2 c (g1 a)) a) : Prism<'a,'c>

        static member (>->) (Lens, (f, t): Isomorphism<'b,'c>) =
            fun ((g, s): Lens<'a,'b>) ->
                (fun a -> f (g a)),
                (fun c a -> s (t c) a) : Lens<'a,'c>

        static member (>->) (Lens, (f, t): Epimorphism<'b,'c>) =
            fun ((g, s): Lens<'a,'b>) ->
                (fun a -> f (g a)),
                (fun c a -> s (t c) a) : Prism<'a,'c>

    /// Compose a lens with an optic or morphism.
    let inline lens l o =
        (Lens >-> o) l

    /// Static overloads of the composition function for prisms (>?>).
    /// These functions do not generally need to be called directly, but will
    /// be used when calling Compose.optic.
    type Prism =
        | Prism with

        static member (>?>) (Prism, (g2, s2): Lens<'b,'c>) =
            fun ((g1, s1): Prism<'a,'b>) ->
                (fun a -> Option.map g2 (g1 a)),
                (fun c a -> Option.map (s2 c) (g1 a) |> function | Some b -> s1 b a
                                                                 | _ -> a) : Prism<'a,'c>

        static member (>?>) (Prism, (g2, s2): Prism<'b,'c>) =
            fun ((g1, s1): Prism<'a,'b>) ->
                (fun a -> Option.bind g2 (g1 a)),
                (fun c a -> Option.map (s2 c) (g1 a) |> function | Some b -> s1 b a
                                                                 | _ -> a) : Prism<'a,'c>

        static member (>?>) (Prism, (f, t): Isomorphism<'b,'c>) =
            fun ((g, s): Prism<'a,'b>) ->
                (fun a -> Option.map f (g a)),
                (fun c a -> s (t c) a) : Prism<'a,'c>

        static member (>?>) (Prism, (f, t): Epimorphism<'b,'c>) =
            fun ((g, s): Prism<'a,'b>) ->
                (fun a -> Option.bind f (g a)),
                (fun c a -> s (t c) a) : Prism<'a,'c>

    /// Compose a prism with an optic or morphism.
    let inline prism p o =
        (Prism >?> o) p

    (* Obsolete

       Backwards compatibility shims to make the 2.x-> 3.x transition
       less painful, providing functionally equivalent options where possible.

       To be removed for 9.x releases. *)

    /// Compose a lens with a lens, giving a lens.
    [<Obsolete ("Use Compose.lens instead.")>]
    let inline lensWithLens l1 l2 =
        lens l1 l2

    /// Compose a lens with a prism, giving a prism.
    [<Obsolete ("Use Compose.lens instead.")>]
    let inline lensWithPrism l1 p1 =
        lens l1 p1

    /// Compose a lens with an isomorphism, giving a lens.
    [<Obsolete ("Use Compose.lens instead.")>]
    let inline lensWithIsomorphism l1 i1 =
        lens l1 i1

    /// Compose a lens with a partial isomorphism, giving a prism.
    [<Obsolete ("Use Compose.lens instead.")>]
    let inline lensWithPartialIsomorphism l1 e1 =
        lens l1 e1

    /// Compose a prism and a lens, giving a prism.
    [<Obsolete ("Use Compose.prism instead.")>]
    let inline prismWithLens p1 l1 =
        prism p1 l1

    /// Compose a prism with a prism, giving a prism.
    [<Obsolete ("Use Compose.prism instead.")>]
    let inline prismWithPrism p1 p2 =
        prism p1 p2

    /// Compose a prism with an isomorphism, giving a prism.
    [<Obsolete ("Use Compose.prism instead.")>]
    let inline prismWithIsomorphism p1 i1 =
        prism p1 i1

    /// Compose a prism with a partial isomorphism, giving a prism.
    [<Obsolete ("Use Compose.prism instead.")>]
    let inline prismWithPartialIsomorphism p1 e1 =
        prism p1 e1

    /// Compose a total lens and a total lens, giving a total lens
    [<Obsolete ("Use Compose.lens instead.")>]
    let totalLensTotalLens (l1: Lens<'a,'b>) (l2: Lens<'b,'c>) : Lens<'a,'c> =
        lens l1 l2

    /// Compose a total lens and a partial lens, giving a partial lens
    [<Obsolete ("Use Compose.lens instead.")>]
    let totalLensPartialLens (l1: Lens<'a,'b>) (p1: Prism<'b,'c>) : Prism<'a,'c> =
        lens l1 p1

    /// Compose a partial lens and a total lens, giving a partial lens
    [<Obsolete ("Use Compose.prism instead.")>]
    let partialLensTotalLens (p1: Prism<'a,'b>) (l1: Lens<'b,'c>) : Prism<'a,'c> =
        prism p1 l1

    /// Compose two partial lenses, giving a partial lens
    [<Obsolete ("Use Compose.prism instead.")>]
    let partialLensPartialLens (p1: Prism<'a,'b>) (p2: Prism<'b,'c>) : Prism<'a,'c> =
        prism p1 p2

    /// Compose a total lens with a total isomorphism, giving a total lens
    [<Obsolete ("Use Compose.lens instead.")>]
    let totalLensTotalIsomorphism (l1: Lens<'a,'b>) (i1: Isomorphism<'b,'c>) : Lens<'a,'c> =
        lens l1 i1

    /// Compose a total lens with a partial isomorphism, giving a partial lens
    [<Obsolete ("Use Compose.lens instead.")>]
    let totalLensPartialIsomorphism (l1: Lens<'a,'b>) (p1: Epimorphism<'b,'c>) : Prism<'a,'c> =
        lens l1 p1

    /// Compose a partial lens with a total isomorphism, giving a partial lens
    [<Obsolete ("Use Compose.prism instead.")>]
    let partialLensTotalIsomorphism (p1: Prism<'a,'b>) (i1: Isomorphism<'b, 'c>) : Prism<'a,'c> =
        prism p1 i1

    /// Compose a partial lens with a partial isomorphism, giving a partial lens
    [<Obsolete ("Use Compose.prism instead.")>]
    let partialLensPartialIsomorphism (p1: Prism<'a,'b>) (e1: Epimorphism<'b,'c>) : Prism<'a,'c> =
        prism p1 e1

/// Functions for using optics to operate on data structures, using the basic optic
/// operations of get, set and map. The functions are overloaded to take either lenses or
/// prisms, with the return type being inferred.
[<RequireQualifiedAccess>]
module Optic =

    /// Static overloads of the optic get function (^.). These functions do not generally
    /// need to be called directly, but will be used when calling Optic.get.
    type Get =
        | Get with

        static member (^.) (Get, (g, _): Lens<'a,'b>) =
            fun (a: 'a) ->
                g a : 'b

        static member (^.) (Get, (g, _): Prism<'a,'b>) =
            fun (a: 'a) ->
                g a : 'b option

    /// Get a value using an optic.
    let inline get optic target =
        (Get ^. optic) target

    /// Static overloads of the optic set function (^=). These functions do
    /// not generally need to be called directly, but will be used when calling
    /// Optic.set.
    type Set =
        | Set with

        static member (^=) (Set, (_, s): Lens<'a,'b>) =
            fun (b: 'b) ->
                s b : 'a -> 'a

        static member (^=) (Set, (_, s): Prism<'a,'b>) =
            fun (b: 'b) ->
                s b : 'a -> 'a

    /// Set a value using an optic.
    let inline set optic value =
        (Set ^= optic) value

    /// Static overloads of the optic map function (%=). These functions do not generally
    /// need to be called directly, but will be used when calling Optic.map.
    type Map =
        | Map with

        static member (^%) (Map, (g, s): Lens<'a,'b>) =
            fun (f: 'b -> 'b) ->
                (fun a -> s (f (g a)) a) : 'a -> 'a

        static member (^%) (Map, (g, s): Prism<'a,'b>) =
            fun (f: 'b -> 'b) ->
                (fun a -> Option.map f (g a) |> function | Some b -> s b a
                                                         | _ -> a) : 'a -> 'a

    /// Modify a value using an optic.
    let inline map optic f =
        (Map ^% optic) f

/// Functions for creating or using lenses.
[<RequireQualifiedAccess>]
module Lens =

    /// Converts an isomorphism into a lens.
    let ofIsomorphism ((f, t): Isomorphism<'a,'b>) : Lens<'a,'b> =
        f, (fun b _ -> t b)

    (* Obsolete

       Backwards compatibility shims to make the 2.x-> 3.x transition
       less painful, providing functionally equivalent options where possible.

       To be removed for 9.x releases. *)

    /// Get a value using a lens.
    [<Obsolete ("Use Optic.get instead.")>]
    let inline get l =
        Optic.get l

    /// Get a value option using a partial lens
    [<Obsolete ("Use Optic.get instead.")>]
    let getPartial (p: Prism<'a,'b>) =
        Optic.get p

    /// Get a value or a default using a partial lens
    [<Obsolete ("Use Optic.get instead. Compose it with some Option.orElse implementation, not provided by Aether.")>]
    let getPartialOrElse (p: Prism<'a,'b>) =
        fun b -> Optic.get p >> (function | Some b -> b | _ -> b)

    /// Set a value using a lens.
    [<Obsolete ("Use Optic.set instead.")>]
    let inline set l =
        Optic.set l

    /// Set a value using a partial lens
    [<Obsolete ("Use Optic.set instead.")>]
    let setPartial (p: Prism<'a,'b>) =
        Optic.set p

    /// Map a value using a lens.
    [<Obsolete ("Use Optic.map instead.")>]
    let inline map l =
        Optic.map l

    /// Modify a value using a partial lens
    [<Obsolete ("Use Optic.map instead.")>]
    let mapPartial (p: Prism<'a,'b>) =
        Optic.map p

/// Functions for creating or using prisms.
[<RequireQualifiedAccess>]
module Prism =

    /// Converts an epimorphism into a prism.
    let ofEpimorphism ((f, t): Epimorphism<'a,'b>) : Prism<'a,'b> =
        f, (fun b _ -> t b)

    (* Obsolete

       Backwards compatibility shims to make the 2.x-> 3.x transition
       less painful, providing functionally equivalent options where possible.

       To be removed for 9.x releases. *)

    /// Get a value using a prism.
    [<Obsolete ("Use Optic.get instead.")>]
    let inline get p =
        Optic.get p

    /// Set a value using a prism.
    [<Obsolete ("Use Optic.set instead.")>]
    let inline set p =
        Optic.set p

    /// Map a value using a prism.
    [<Obsolete ("Use Optic.map instead.")>]
    let inline map p =
        Optic.map p

/// Various optics implemented for common types such as tuples,
/// lists and maps, along with an identity lens.
[<AutoOpen>]
module Optics =

    // Lens for the identity function (does not change the focus of operation).
    let id_ : Lens<'a,'a> =
        (fun x -> x),
        (fun x _ -> x)

    /// Isomorphism between a boxed and unboxed type.
    let box_<'a> : Isomorphism<obj,'a> =
        unbox<'a>, box

    /// Lens to the first item of a tuple.
    let fst_ : Lens<('a * 'b),'a> =
        fst,
        (fun a t -> a, snd t)

    /// Lens to the second item of a tuple.
    let snd_ : Lens<('a * 'b),'b> =
        snd,
        (fun b t -> fst t, b)

    [<RequireQualifiedAccess>]
    module Array =

        /// Isomorphism to an list.
        let list_ : Isomorphism<'v[], 'v list> =
            Array.toList,
            Array.ofList

    [<RequireQualifiedAccess>]
    module Choice =

        /// Prism to Choice1Of2.
        let choice1Of2_ : Prism<Choice<_,_>, _> =
            (fun x ->
                match x with
                | Choice1Of2 v -> Some v
                | _ -> None),
            (fun v x ->
                match x with
                | Choice1Of2 _ -> Choice1Of2 v
                | _ -> x)

        /// Prism to Choice2Of2.
        let choice2Of2_ : Prism<Choice<_,_>, _> =
            (fun x ->
                match x with
                | Choice2Of2 v -> Some v
                | _ -> None),
            (fun v x ->
                match x with
                | Choice2Of2 _ -> Choice2Of2 v
                | _ -> x)

    [<RequireQualifiedAccess>]
    module Result =

        /// Prism to Ok.
        let ok_ : Prism<Result<_,_>, _> =
            (fun x ->
                match x with
                | Ok v -> Some v
                | _ -> None),
            (fun v x ->
                match x with
                | Ok _ -> Ok v
                | _ -> x)

        /// Prism to Error.
        let error_ : Prism<Result<_,_>, _> =
            (fun x ->
                match x with
                | Error v -> Some v
                | _ -> None),
            (fun v x ->
                match x with
                | Error _ -> Error v
                | _ -> x)

    [<RequireQualifiedAccess>]
    module List =

        /// Prism to the head of a list.
        let head_ : Prism<'v list, 'v> =
            (function | h :: _ -> Some h
                      | _ -> None),
            (fun v ->
                function | _ :: t -> v :: t
                         | l -> l)

        /// Prism to an indexed position in a list.
        let pos_ (i: int) : Prism<'v list, 'v> =
#if NET35
            (function | l when List.length l > i -> Some (List.nth l i)
#else
            (function | l when List.length l > i -> Some (List.item i l)
#endif
                      | _ -> None),
            (fun v l ->
                List.mapi (fun i' x -> if i = i' then v else x) l)

        /// Prism to the tail of a list.
        let tail_ : Prism<'v list, 'v list> =
            (function | _ :: t -> Some t
                      | _ -> None),
            (fun t ->
                function | h :: _ -> h :: t
                         | [] -> [])

        /// Isomorphism to an array.
        let array_ : Isomorphism<'v list, 'v[]> =
            List.toArray,
            List.ofArray

    [<RequireQualifiedAccess>]
    module Map =

        /// Prism to a value associated with a key in a map.
        let key_ (k: 'k) : Prism<Map<'k,'v>,'v> =
            Map.tryFind k,
            (fun v x ->
                if Map.containsKey k x then Map.add k v x else x)

        /// Lens to a value option associated with a key in a map.
        let value_ (k: 'k) : Lens<Map<'k,'v>, 'v option> =
            Map.tryFind k,
            (fun v x ->
                match v with
                | Some v -> Map.add k v x
                | _ -> Map.remove k x)

        /// Weak Isomorphism to an array of key-value pairs.
        let array_ : Isomorphism<Map<'k,'v>, ('k * 'v)[]> =
            Map.toArray,
            Map.ofArray

        /// Weak Isomorphism to a list of key-value pairs.
        let list_ : Isomorphism<Map<'k,'v>, ('k * 'v) list> =
            Map.toList,
            Map.ofList

    [<RequireQualifiedAccess>]
    module Option =

        /// Prism to the value in an Option.
        let value_ : Prism<'v option, 'v> =
            id,
            (fun v ->
                function | Some _ -> Some v
                         | None -> None)

(* Obsolete

    Backwards compatibility shims to make the 2.x-> 3.x transition
    less painful, providing functionally equivalent options where possible.

    To be removed for 9.x releases. *)

/// Identity lens returning the original item regardless of modifiction
[<Obsolete ("Use Optics.id_ instead.")>]
let id_ : Lens<'a,'a> =
    Optics.id_

/// First item of a tuple giving a total lens
[<Obsolete ("Use Optics.fst_ instead.")>]
let fst_ : Lens<('a * 'b),'a> =
    Optics.fst_

/// Second item of a tuple giving a total lens
[<Obsolete ("Use Optics.snd_ instead.")>]
let snd_ : Lens<('a * 'b),'b> =
    Optics.snd_

/// Head of a list giving a partial lens
[<Obsolete ("Use Optics.List.head_ instead.")>]
let head_ : Prism<'v list, 'v> =
    Optics.List.head_

/// Position of a list giving a partial lens
[<Obsolete ("Use Optics.List.pos_ instead.")>]
let pos_ (i: int) : Prism<'v list, 'v> =
    Optics.List.pos_ i

/// Tail of a list giving a partial lens
[<Obsolete ("Use Optics.List.tail_ instead.")>]
let tail_ : Prism<'v list, 'v list> =
    Optics.List.tail_

/// Key of a map giving a partial lens
[<Obsolete ("Use Optics.Map.key_ instead.")>]
let key_ (k: 'k) : Prism<Map<'k,'v>,'v> =
    Optics.Map.key_ k


/// Optional custom operators for working with optics. Provides more concise
/// syntactic options for working with the functions in the `Compose` and
/// `Optic` modules.
module Operators =

    /// Compose a lens with an optic or morphism.
    let inline (>->) l o =
        Compose.lens l o

    /// Compose a prism with an optic or morphism.
    let inline (>?>) p o =
        Compose.prism p o

    /// Get a value using an optic.
    let inline (^.) target optic =
        Optic.get optic target

    /// Set a value using an optic.
    let inline (^=) value optic =
        Optic.set optic value

    /// Modify a value using an optic.
    let inline (^%) f optic =
        Optic.map optic f

    (* Obsolete

       Backwards compatibility shims to make the 2.x-> 3.x transition
       less painful, providing functionally equivalent options where possible.

       To be removed for 9.x releases. *)

    /// Compose a lens and a lens, giving a lens.
    [<Obsolete ("Use >-> instead.")>]
    let inline (>-->) l1 l2 =
        Compose.lens l1 l2

    /// Compose a lens and a prism, giving a prism.
    [<Obsolete ("Use >-> instead.")>]
    let inline (>-?>) l1 l2 =
        Compose.lens l1 l2

    /// Compose a lens with an isomorphism, giving a lens.
    [<Obsolete ("Use >-> instead.")>]
    let inline (<-->) l i =
        Compose.lens l i

    /// Compose a total lens with a partial isomorphism, giving a prism.
    [<Obsolete ("Use >-> instead.")>]
    let inline (<-?>) l i =
        Compose.lens l i

    /// Compose a prism and a lens, giving a prism.
    [<Obsolete ("Use >?> instead.")>]
    let inline (>?->) l1 l2 =
        Compose.prism l1 l2

    /// Compose a prism with a prism, giving a prism.
    [<Obsolete ("Use >?> instead.")>]
    let inline (>??>) l1 l2 =
        Compose.prism l1 l2

    /// Compose a prism with an isomorphism, giving a prism.
    [<Obsolete ("Use >?> instead.")>]
    let inline (<?->) l i =
        Compose.prism l i

    /// Compose a prism with a partial isomorphism, giving a prism.
    [<Obsolete ("Use >?> instead.")>]
    let inline (<??>) l i =
        Compose.prism l i

    /// Get a value using a prism.
    [<Obsolete ("Use ^. instead.")>]
    let inline (^?.) a p =
        Optic.get p a

    /// Set a value using a prism.
    [<Obsolete ("Use ^= instead.")>]
    let inline (^?=) b p =
        Optic.set p b

    /// Modify a value using a total lens
    [<Obsolete ("Use ^% instead.")>]
    let (^%=) (f: 'b -> 'b) (l: Lens<'a,'b>) : 'a -> 'a =
        Optic.map l f

    /// Modify a value using a prism.
    [<Obsolete ("Use ^% instead.")>]
    let inline (^?%=) f p =
        Optic.map p f
