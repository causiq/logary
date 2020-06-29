namespace Logary.Internals.Chiron

open System.Collections.Generic

type JsonMemberType =
    | Object
    | Array
    | String
    | Number
    | Bool
    | Null

type [<StructuralEquality;NoComparison>] Json =
    | Object of properties: JsonObject
    | Array of elements: Json list
    | String of string
    | Number of number:string
    | True
    | False
    | Null

and [<CustomEquality;NoComparison>] JsonObject =
    | WriteObject of propList: (string * Json) list
    | ReadObject of propList: (string * Json) list * propMap: Map<string,Json>
    override x.Equals(o) =
        match o with
        | :? JsonObject as y -> (x :> System.IEquatable<JsonObject>).Equals(y)
        | _ -> false
    override x.GetHashCode() =
        match x with
        | WriteObject ps -> Map.ofList (List.rev ps) |> hash
        | ReadObject (_, mps) -> mps |> hash
    interface System.IEquatable<JsonObject> with
        member x.Equals(y) =
            match x, y with
            | WriteObject xps, WriteObject yps -> Map.ofList (List.rev xps) = Map.ofList (List.rev yps)
            | ReadObject (_, mps), WriteObject ps -> Map.ofList (List.rev ps) = mps
            | WriteObject ps, ReadObject (_, mps) -> Map.ofList (List.rev ps) = mps
            | ReadObject (_, xps), ReadObject (_, yps) -> xps = yps

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module JsonMemberType =
    let ofJson = function
        | Json.Object _ -> JsonMemberType.Object
        | Json.Array _ -> JsonMemberType.Array
        | Json.String _ -> JsonMemberType.String
        | Json.Number _ -> JsonMemberType.Number
        | Json.True -> JsonMemberType.Bool
        | Json.False -> JsonMemberType.Bool
        | Json.Null -> JsonMemberType.Null

    let describe = function
        | JsonMemberType.Object -> "an object"
        | JsonMemberType.Array -> "an array"
        | JsonMemberType.String -> "a string"
        | JsonMemberType.Number -> "a number"
        | JsonMemberType.Bool -> "a boolean"
        | JsonMemberType.Null -> "null"

type JsonFailureTag =
    | PropertyTag of propertyName: string
    | IndexTag of index: uint32
    | ChoiceTag of choice: uint32
type JsonFailureReason =
    | OtherError of err: exn
    | MessageTypeUnknown of name: string
    | TypeMismatch of expected: JsonMemberType * actual: JsonMemberType
    | DeserializationError of targetType: System.Type * err: exn
    | InvalidJson of err: string
    | NoInput
type JsonFailure =
    | Tagged of tag: JsonFailureTag * jFail: JsonFailure
    | SingleFailure of jFail: JsonFailureReason
    | PairedFailures of leftFail: JsonFailure * rightFail: JsonFailure
    | MultipleFailures of jFails: JsonFailure list
type ChironErrorPolicy =
    | StopOnFirstError
    | ContinueOnError

type JsonResult<'a> =
    | JPass of 'a
    | JFail of JsonFailure

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module JsonFailureTag =
    let toString = function
        | PropertyTag p -> p
        | IndexTag i -> System.String.Concat ("[", string i, "]")
        | ChoiceTag c -> System.String.Concat ("(Choice #", string (c + 1u), ")")

    let toTagPathString prefixO tag =
        match prefixO, tag with
        | Some prefix, PropertyTag p -> System.String.Concat (prefix, ".", p)
        | Some prefix, IndexTag i -> System.String.Concat (prefix, "[", string i, "]")
        | Some prefix, ChoiceTag c -> System.String.Concat (prefix, "(Choice #", string (c + 1u), ")")
        | None, _ -> toString tag

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module JsonFailureReason =
    let toString = function
        | OtherError e -> string e
        | MessageTypeUnknown typ -> "Unknown message type: '" + typ + "'"
        | TypeMismatch (e,a) -> System.String.Concat ("Expected to find ", JsonMemberType.describe e, ", but instead found ", JsonMemberType.describe a)
        | DeserializationError (t,e) -> System.String.Concat ("Unable to deserialize value as '", t.FullName, "': ", string e)
        | InvalidJson e -> "Invalid JSON, failed to parse: " + e
        | NoInput -> "No input was provided"

    let toStringWithPath pathO jFail =
        let path = (match pathO with Some p -> p; | _ -> System.String.Empty)//  |> Option.defaultValue System.String.Empty
        System.String.Concat (path, ": ", toString jFail)

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module JsonFailure =
    let mappend jFail1 jFail2 =
        PairedFailures (jFail1, jFail2)
    let tag (t: JsonFailureTag) (f: JsonFailure) =
        Tagged (t, f)
    let toStrings jFail =
        let rec inner (prefix: string option) = function
            | Tagged (tag, innerFail) ->
                inner (JsonFailureTag.toTagPathString prefix tag |> Some) innerFail
            | SingleFailure jfr ->
                [ JsonFailureReason.toStringWithPath prefix jfr ]
            | PairedFailures (jFail1, jFail2) ->
                [ jFail1; jFail2 ] |> List.collect (inner prefix)
            | MultipleFailures jFails ->
                jFails |> List.rev |> List.collect (inner prefix)
        inner None jFail
    let summarize jFail =
        match toStrings jFail with
        | [] -> "No errors"
        | [e] -> sprintf "Found 1 error:\n  %s" <| e
        | errs ->
            let sb = System.Text.StringBuilder()
            sb.AppendLine(sprintf "Found %i errors:" (List.length errs)) |> ignore
            errs |> List.iter (fun e -> sb.Append("  ").AppendLine(e) |> ignore)
            sb.ToString()

module JsonResult =
    let inline pass a = JPass a
    let inline fail f = JFail f
    let inline fails fs = JFail (MultipleFailures fs)

    let getOrThrow : JsonResult<'a> -> 'a = function
        | JPass x -> x
        | JFail f -> failwith (JsonFailure.toStrings f |> String.concat "\n")

    let toResult : JsonResult<'a> -> Result<'a, JsonFailure> = function
        | JPass x -> Ok x
        | JFail f -> Error f

    let raise e = fail (SingleFailure (OtherError e))
    let typeMismatch expected json = fail (SingleFailure (TypeMismatch (expected, JsonMemberType.ofJson json)))
    let deserializationError<'a> exn : JsonResult<'a> = fail (SingleFailure (DeserializationError (typeof<'a>, exn)))
    let invalidJson err : JsonResult<'a> = fail (SingleFailure (InvalidJson err))
    let noInput<'a> : JsonResult<'a> = fail (SingleFailure NoInput)
    let messageTypeUnknown f : JsonResult<'a> = fail (SingleFailure (MessageTypeUnknown f))
    let failWithTag t f : JsonResult<'a> = fail (Tagged (t, f))

    let inline bind (a2bR: 'a -> JsonResult<'b>) (aR : JsonResult<'a>) : JsonResult<'b> =
        match aR with
        | JPass a -> a2bR a
        | JFail x -> fail x

    let inline map (a2b: 'a -> 'b) (aR : JsonResult<'a>) : JsonResult<'b> =
        match aR with
        | JPass a -> pass (a2b a)
        | JFail x -> fail x

    let inline apply (aR: JsonResult<'a>) (a2Rb: JsonResult<'a -> 'b>) : JsonResult<'b> =
        match a2Rb, aR with
        | JPass a2b, JPass a -> pass (a2b a)
        | JFail e1, JFail e2 -> fail (JsonFailure.mappend e1 e2)
        | JFail e, _ | _, JFail e -> fail e

    let applyDelayedWithPolicy (policy: ChironErrorPolicy) (c2aR: 'c -> JsonResult<'a>) (c: 'c) (a2Rb: JsonResult<'a -> 'b>) : JsonResult<'b> =
        match a2Rb with
        | JPass a2b ->
            match c2aR c with
            | JPass a -> pass (a2b a)
            | JFail e -> fail e
        | JFail e1 ->
            match policy with
            | StopOnFirstError -> fail e1
            | ContinueOnError ->
                match c2aR c with
                | JPass a -> fail e1
                | JFail e2 -> fail (JsonFailure.mappend e1 e2)

    let applyDelayedWithPolicyAlt (policy: ChironErrorPolicy) : ('c -> JsonResult<'a>) -> 'c -> JsonResult<'a -> 'b> -> JsonResult<'b> =
        match policy with
        | StopOnFirstError ->
            fun c2aR c a2Rb ->
                match a2Rb with
                | JPass a2b ->
                    match c2aR c with
                    | JPass a -> pass (a2b a)
                    | JFail e -> fail e
                | JFail e1 -> fail e1
        | ContinueOnError ->
            fun c2aR c a2Rb -> apply (c2aR c) a2Rb

    let inline mapError (f2b: JsonFailure -> 'a) (aR: JsonResult<'a>): JsonResult<'a> =
        match aR with
        | JPass a -> JPass a
        | JFail x -> JPass (f2b x)

    let foldBind (folder: 'state -> 'item -> JsonResult<'state>) (state: 'state) (xsJ: seq<'item>): JsonResult<'state> =
        Seq.fold (fun aJ xJ -> aJ |> bind (fun a -> folder a xJ))
                 (pass state)
                 xsJ

    let ofOption failureReason xO =
        match xO with
        | None ->
          JFail (SingleFailure failureReason)
        | Some st ->
          JPass st

type Decoder<'s,'a> = 's -> JsonResult<'a>

type ObjectReader<'a> = Decoder<JsonObject,'a>
type ObjectBuilder<'a> = 'a -> JsonObject -> JsonObject

type JsonDecoder<'a> = Decoder<Json,'a>
type JsonEncoder<'a> = 'a -> Json

module Decoder =
    let alwaysPass (a: 'a) : Decoder<'s,'a> =
        fun s -> JsonResult.pass a

    let alwaysFail (e: JsonFailure) : Decoder<'s,'a> =
        fun s -> JsonResult.fail e

    let always result : Decoder<'s,'a> =
        fun s -> result

    let inline bind (a2s2bR: 'a -> Decoder<'s,'b>) (s2aR: Decoder<'s,'a>): Decoder<'s,'b> =
        fun s ->
            s2aR s
            |> JsonResult.bind (fun a -> a2s2bR a s)

    let inline map (a2b: 'a -> 'b) (s2aR: Decoder<'s,'a>) : Decoder<'s,'b> =
        fun s ->
            s2aR s
            |> JsonResult.map a2b

    let inline compose (b2cR: Decoder<'b,'c>) (a2bR: Decoder<'a,'b>) : Decoder<'a,'c> =
        fun a ->
            a2bR a
            |> JsonResult.bind b2cR

    let inline applyWithContinueOnError (s2aR: Decoder<'s,'a>) (s2Ra2b: Decoder<'s,'a -> 'b>) : Decoder<'s,'b> =
        fun s ->
            match s2Ra2b s, s2aR s with
            | JPass a2b, JPass a -> JsonResult.pass (a2b a)
            | JFail e1, JFail e2 -> JsonResult.fail (JsonFailure.mappend e1 e2)
            | JFail e, _ | _, JFail e -> JsonResult.fail e

    let inline applyWithStopOnFirstError (s2aR: Decoder<'s,'a>) (s2Ra2b: Decoder<'s,'a -> 'b>) : Decoder<'s,'b> =
        fun s ->
            match s2Ra2b s with
            | JPass a2b -> s2aR s |> JsonResult.map a2b
            | JFail e -> JsonResult.fail e

    let applyWithPolicy (policy: ChironErrorPolicy) : Decoder<'s,'a> -> Decoder<'s,'a -> 'b> -> Decoder<'s,'b> =
        match policy with
        | StopOnFirstError -> applyWithStopOnFirstError
        | ContinueOnError -> applyWithContinueOnError

    let inline apply (s2aR: Decoder<'s,'a>) (s2Ra2b: Decoder<'s,'a -> 'b>) : Decoder<'s,'b> =
        applyWithContinueOnError s2aR s2Ra2b

    let map2 (a2b2c: 'a -> 'b -> 'c) (s2aR: Decoder<'s,'a>) (s2bR: Decoder<'s,'b>) : Decoder<'s,'c> =
        map a2b2c s2aR
        |> apply s2bR

    let map3 (a2b2c2d: 'a -> 'b -> 'c -> 'd) (s2aR: Decoder<'s,'a>) (s2bR: Decoder<'s,'b>) (s2cR: Decoder<'s,'c>) : Decoder<'s,'d> =
        map a2b2c2d s2aR
        |> apply s2bR
        |> apply s2cR

    let map4 (a2b2c2d2x: 'a -> 'b -> 'c -> 'd -> 'x) (s2aR: Decoder<'s,'a>) (s2bR: Decoder<'s,'b>) (s2cR: Decoder<'s,'c>) (s2dR: Decoder<'s,'d>) : Decoder<'s,'x> =
        map a2b2c2d2x s2aR
        |> apply s2bR
        |> apply s2cR
        |> apply s2dR

    let map5 (a2b2c2d2x2y: 'a -> 'b -> 'c -> 'd -> 'x -> 'y) (s2aR: Decoder<'s,'a>) (s2bR: Decoder<'s,'b>) (s2cR: Decoder<'s,'c>) (s2dR: Decoder<'s,'d>) (s2xR: Decoder<'s,'x>) : Decoder<'s,'y> =
        map a2b2c2d2x2y s2aR
        |> apply s2bR
        |> apply s2cR
        |> apply s2dR
        |> apply s2xR

    let map6 (a2b2c2d2x2y2z: 'a -> 'b -> 'c -> 'd -> 'x -> 'y -> 'z) (s2aR: Decoder<'s,'a>) (s2bR: Decoder<'s,'b>) (s2cR: Decoder<'s,'c>) (s2dR: Decoder<'s,'d>) (s2xR: Decoder<'s,'x>) (s2yR: Decoder<'s,'y>) : Decoder<'s,'z> =
        map a2b2c2d2x2y2z s2aR
        |> apply s2bR
        |> apply s2cR
        |> apply s2dR
        |> apply s2xR
        |> apply s2yR

    let withPropertyTag p (s2aR : Decoder<'s,'a>) : Decoder<'s,'a> =
        fun s ->
            match s2aR s with
            | JPass a -> JsonResult.pass a
            | JFail f -> JsonResult.failWithTag (PropertyTag p) f

    let withIndexTag i (s2aR : Decoder<'s,'a>) : Decoder<'s,'a> =
        fun s ->
            match s2aR s with
            | JPass a -> JsonResult.pass a
            | JFail f -> JsonResult.failWithTag (IndexTag i) f

    let withChoiceTag c (s2aR : Decoder<'s,'a>) : Decoder<'s,'a> =
        fun s ->
            match s2aR s with
            | JPass a -> JsonResult.pass a
            | JFail f -> JsonResult.failWithTag (ChoiceTag c) f

    let inline fromThrowingConverter (convert: 's -> 'a) : Decoder<'s,'a> =
        fun s ->
            try
                JsonResult.pass (convert s)
            with e -> JsonResult.deserializationError<'a> e

    let inline fromNodaTime (convert: _ -> NodaTime.Text.ParseResult<_>): Decoder<'s, 'a> =
        fun s ->
            try
                let res = convert s
                if res.Success then
                    JsonResult.pass res.Value
                else
                    JsonResult.deserializationError<'a> res.Exception
            with e -> JsonResult.deserializationError<'a> e

    let inline withPropertyTagInline p (s2aR : Decoder<'s,'a>) : Decoder<'s,'a> =
        fun s ->
            match s2aR s with
            | JPass a -> JsonResult.pass a
            | JFail f -> JsonResult.failWithTag (PropertyTag p) f

    let inline withIndexTagInline i (s2aR : Decoder<'s,'a>) : Decoder<'s,'a> =
        fun s ->
            match s2aR s with
            | JPass a -> JsonResult.pass a
            | JFail f -> JsonResult.failWithTag (IndexTag i) f

    let inline withChoiceTagInline c (s2aR : Decoder<'s,'a>) : Decoder<'s,'a> =
        fun s ->
            match s2aR s with
            | JPass a -> JsonResult.pass a
            | JFail f -> JsonResult.failWithTag (ChoiceTag c) f

    let inline fromThrowingConverterInline (convert: 's -> 'a) : Decoder<'s,'a> =
        fun s ->
            try
                JsonResult.pass (convert s)
            with e -> JsonResult.deserializationError<'a> e

module Operators =
    let inline (<!>) (a2b: 'a -> 'b) (s2aR: Decoder<'s,'a>) = Decoder.map a2b s2aR
    let inline (<*>) (s2Ra2b: Decoder<'s,'a -> 'b>) (s2aR: Decoder<'s,'a>) = Decoder.apply s2aR s2Ra2b
    let inline (>=>) (a2bR: Decoder<'a,'b>) (b2cR: Decoder<'b,'c>) = Decoder.compose b2cR a2bR
    let inline (>->) (s2aR: Decoder<'s,'a>) (a2b: 'a -> 'b) = Decoder.map a2b s2aR

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module JsonObject =
    let empty = WriteObject []

    module PropertyList =
        let inline add k v ps = (k, v) :: ps
        let remove k ps = List.filter (fun kvp -> fst kvp <> k) ps
        let upsert k v ps =
            let mutable found = false
            let newList = List.map (fun kvp -> if fst kvp = k then found <- true; (fst kvp, v) else kvp) ps
            if not found then
                add k v ps
            else
                newList

        let tryFind k ps = List.tryPick (fun kvp -> if fst kvp = k then Some (snd kvp) else None) ps

    module PropertyMap =
        let inline add k v ps = Map.add k v ps
        let inline remove k ps = Map.remove k ps
        let inline upsert k v ps = Map.add k v ps

        let inline tryFind k ps = Map.tryFind k ps

    let add k v = function
        | WriteObject ps -> WriteObject (PropertyList.add k v ps)
        | ReadObject (ps, mps) ->
            ReadObject (PropertyList.add k v ps, PropertyMap.add k v mps)

    let remove k = function
        | WriteObject ps -> WriteObject (PropertyList.remove k ps)
        | ReadObject (ps, mps) -> ReadObject (PropertyList.remove k ps, PropertyMap.remove k mps)

    let upsert k v = function
        | WriteObject ps -> WriteObject (PropertyList.upsert k v ps)
        | ReadObject (ps, mps) ->
            ReadObject (PropertyList.upsert k v ps, PropertyMap.upsert k v mps)

    let tryFind k = function
        | WriteObject ps -> PropertyList.tryFind k ps
        | ReadObject (_, mps) -> PropertyMap.tryFind k mps

    let find k jsonObj =
        match tryFind k jsonObj with
        | Some v -> JsonResult.pass v
        | None -> JsonResult.messageTypeUnknown k

    let optimizeRead = function
        | WriteObject ps -> ReadObject (ps, Map.ofList (List.rev ps))
        | o -> o

    let optimizeAppend = function
        | ReadObject (ps, mps) -> WriteObject ps
        | o -> o

    let mapToList mps = Map.toList mps |> List.rev

    let listToMap ps = List.rev ps |> Map.ofList

    let dedupeWithMap kvps m =
        if List.length kvps = Map.count m then
            kvps
        else
            let hs = System.Collections.Generic.HashSet ()
            List.foldBack (fun (k,_) s -> if hs.Add(k) then (k, Map.find k m) :: s else s) kvps []

    let dedupeList kvps =
        dedupeWithMap kvps (listToMap kvps)

    let removeDuplicates = function
        | WriteObject ps -> WriteObject (dedupeList ps)
        | ReadObject (ps, mps) ->
            if List.length ps <> Map.count mps then
                ReadObject (dedupeWithMap ps mps, mps)
            else
                ReadObject (ps, mps)

    //
    let toPropertyListWithCustomKeyQuick (parse: Decoder<string,'a>) (decode: Decoder<Json,'b>) : Decoder<JsonObject,('a * 'b) list> = function
        | WriteObject ps
        | ReadObject (ps, _) ->
            let rec loop agg lastK lastV ps =
                match Decoder.withPropertyTag lastK parse lastK with
                | JPass k ->
                    match Decoder.withPropertyTag lastK decode lastV with
                    | JPass v ->
                        let nextAgg = (k,v)::agg
                        match ps with
                        | [] -> JsonResult.pass nextAgg
                        | (nextK,nextV)::nextPs -> loop nextAgg nextK nextV nextPs
                    | JFail errs -> JsonResult.fail errs
                | JFail errs -> JsonResult.fail errs
            match ps with
            | [] -> JsonResult.pass []
            | (initK,initV)::initPs -> loop [] initK initV initPs

    let toPropertyListWithCustomKey (parse: Decoder<string,'a>) (decode: Decoder<Json,'b>) : Decoder<JsonObject,('a * 'b) list> =
        let idλ = (do ()); fun (x: JsonFailure list) -> x
        function
        | WriteObject ps
        | ReadObject (ps, _) ->
            let rec goodPath agg lastK lastV ps =
                let kR = Decoder.withPropertyTag lastK parse lastK
                let vR = Decoder.withPropertyTag lastK decode lastV
                match kR, vR with
                | JPass k, JPass v ->
                    let nextAgg = (k,v)::agg
                    match ps with
                    | [] -> JsonResult.pass nextAgg
                    | (nextK,nextV)::nextPs -> goodPath nextAgg nextK nextV nextPs
                | JFail errs1, JFail errs2 ->
                    badPathTransfer [errs2;errs1] ps
                | JFail errs, _
                | _, JFail errs ->
                    badPathTransfer [errs] ps
            and badPath (aggErrs: JsonFailure list) lastK lastV ps =
                let kR = Decoder.withPropertyTag lastK parse lastK
                let vR = Decoder.withPropertyTag lastK decode lastV
                match kR, vR with
                | JFail errs1, JFail errs2 ->
                    badPathTransfer (errs2::errs1::aggErrs) ps
                | JFail errs, _
                | _, JFail errs ->
                    badPathTransfer (errs::aggErrs) ps
                | _ ->
                    badPathTransfer aggErrs ps
            and badPathTransfer aggErrs ps =
                match ps with
                | [] -> JsonResult.fails aggErrs
                | (nextK,nextV)::nextPs -> badPath aggErrs nextK nextV nextPs
            match ps with
            | [] -> JPass []
            | (initK,initV)::initPs -> goodPath [] initK initV initPs

    let toPropertyListWithQuick (decode: Decoder<Json,'b>) = function
        | WriteObject ps
        | ReadObject (ps, _) ->
            let rec loop agg k lastV ps =
                match Decoder.withPropertyTag k decode lastV with
                | JPass v ->
                    let nextAgg = (k,v)::agg
                    match ps with
                    | [] -> JsonResult.pass nextAgg
                    | (nextK,nextV)::nextPs -> loop nextAgg nextK nextV nextPs
                | JFail errs -> JsonResult.fail errs
            match ps with
            | [] -> JsonResult.pass []
            | (initK,initV)::initPs -> loop [] initK initV initPs
    let toPropertyListWith (decode: Decoder<Json,'b>) =
        let idλ = (do ()); fun (x: JsonFailure list) -> x
        function
        | WriteObject ps
        | ReadObject (ps, _) ->
            let rec goodPath agg k lastV ps =
                match Decoder.withPropertyTag k decode lastV with
                | JPass v ->
                    let nextAgg = (k,v)::agg
                    match ps with
                    | [] -> JsonResult.pass nextAgg
                    | (nextK,nextV)::nextPs -> goodPath nextAgg nextK nextV nextPs
                | JFail errs ->
                    badPathTransfer [errs] ps
            and badPath (aggErrs: JsonFailure list) k lastV ps =
                match Decoder.withPropertyTag k decode lastV with
                | JFail errs ->
                    badPathTransfer (errs::aggErrs) ps
                | _ ->
                    badPathTransfer aggErrs ps
            and badPathTransfer aggErrs ps =
                match ps with
                | [] -> JsonResult.fails aggErrs
                | (nextK,nextV)::nextPs -> badPath aggErrs nextK nextV nextPs
            match ps with
            | [] -> JsonResult.pass []
            | (initK,initV)::initPs -> goodPath [] initK initV initPs
    let toPropertyList = function
        | WriteObject ps -> List.rev ps
        | ReadObject (ps, _) -> List.rev ps

    let ofPropertyListWithCustomKey (toString: 'a -> string) (encode: JsonEncoder<'b>) (ps: ('a * 'b) list): JsonObject =
        let rec inner agg = function
            | [] -> WriteObject agg
            | (k,v) :: ps ->
                inner ((toString k, encode v) :: agg) ps
        inner [] ps

    let ofPropertyListWith (encode: JsonEncoder<'a>) (ps: (string * 'a) list): JsonObject =
        ofPropertyListWithCustomKey id encode ps

    let ofPropertyList (ps: (string * Json) list): JsonObject =
        List.rev ps
        |> WriteObject

    //
    let toMapWithCustomKeyQuick (parse: Decoder<string,'k>) (decode: Decoder<Json,'v>) =
        toPropertyListWithCustomKeyQuick parse decode
        |> Decoder.map Map.ofList

    let toMapWithCustomKey (parse: Decoder<string,'k>) (decode: Decoder<Json,'v>) =
        toPropertyListWithCustomKey parse decode
        |> Decoder.map Map.ofList

    let toMapWithQuick (decode: Decoder<Json,'v>) =
        toPropertyListWithQuick decode
        |> Decoder.map Map.ofList

    let toMapWith (decode: Decoder<Json,'v>) =
        toPropertyListWith decode
        |> Decoder.map Map.ofList

    let toMap = function
        | WriteObject ps -> List.rev ps |> Map.ofList
        | ReadObject (_, mps) -> mps

    let ofSeq (s: #seq<string * _>) =
      ReadObject (List.ofSeq s, Map.ofSeq s)

    let ofMap m = ReadObject (mapToList m, m)

    let ofMapWith (encode: JsonEncoder<'a>) (m: Map<string, 'a>): JsonObject =
        let newMap = Map.map (fun _ a -> encode a) m
        ReadObject (mapToList newMap, newMap)

    let ofMapWithCustomKey (toString: 'k -> string) (encode: JsonEncoder<'v>) (m: Map<'k, 'v>): JsonObject =
        let newList =
            mapToList m
            |> List.map (fun (k,v) -> (toString k, encode v))
        ReadObject (newList, Map.ofList newList)

    let ofReadDictWith (encode: JsonEncoder<'a>) (m: IReadOnlyDictionary<string, 'a>): JsonObject =
        let newSeq = Seq.map (fun (KeyValue(k, v)) -> k, encode v) m
        let newList = List.ofSeq newSeq |> List.rev
        let newMap = Map.ofSeq newSeq
        ReadObject (newList, newMap)

    let ofReadDictWithCustomKey (toString: 'k -> string) (encode: JsonEncoder<'a>) (m: IReadOnlyDictionary<'k, 'a>): JsonObject =
        let newSeq = Seq.map (fun (KeyValue(k, v)) -> toString k, encode v) m
        let newList = List.ofSeq newSeq |> List.rev
        let newMap = Map.ofSeq newSeq
        ReadObject (newList, newMap)


    let toJson jObj = Json.Object jObj

module Json =
    let Null: Json = Json.Null

[<AutoOpen>]
module Parsing =
    module Parser =
        open FParsec

        let ws   = spaces // eats any whitespace
        let inline str s = pstring s

        let stringLiteral : Parser<string,unit> =
            let escape =  anyOf "\"\\/bfnrt"
                        |>> function
                            | 'b' -> "\b"
                            | 'f' -> "\u000C"
                            | 'n' -> "\n"
                            | 'r' -> "\r"
                            | 't' -> "\t"
                            | c   -> string c // every other char is mapped to itself

            let unicodeEscape =
                str "u" >>. pipe4 hex hex hex hex (fun h3 h2 h1 h0 ->
                    let hex2int c = (int c &&& 15) + (int c >>> 6)*9 // hex char to int
                    (hex2int h3)*4096 + (hex2int h2)*256 + (hex2int h1)*16 + hex2int h0
                    |> char |> string
                )

            between (str "\"") (str "\"")
                    (stringsSepBy (manySatisfy (fun c -> c <> '"' && c <> '\\'))
                                (str "\\" >>. (escape <|> unicodeEscape)))



        let jstring = stringLiteral |>> Json.String

        let jsonNumOpts = NumberLiteralOptions.AllowFraction ||| NumberLiteralOptions.AllowExponent ||| NumberLiteralOptions.AllowMinusSign ||| NumberLiteralOptions.AllowPlusSign
        let jnumber : Parser<Json,unit> =
            fun stream ->
                let reply = numberLiteralE jsonNumOpts (ErrorMessageList(ErrorMessage.Expected("JSON number"))) stream
                if reply.Status = ReplyStatus.Ok then
                    Reply(Json.Number reply.Result.String)
                else
                    Reply(reply.Status, reply.Error)

        let jtrue  = stringReturn "true"  Json.True
        let jfalse = stringReturn "false" Json.False
        let jnull  = stringReturn "null" Json.Null

        // jvalue, jlist and jobject are three mutually recursive grammar productions.
        // In order to break the cyclic dependency, we make jvalue a parser that
        // forwards all calls to a parser in a reference cell.
        let jvalue, jvalueRef = createParserForwardedToRef() // initially jvalueRef holds a reference to a dummy parser

        let listBetweenStrings sOpen sClose pElement f =
            between (str sOpen) (str sClose) (ws >>. sepBy (pElement .>> ws) (str "," .>> ws) |>> f)

        let keyValue = tuple2 stringLiteral (ws >>. str ":" >>. ws >>. jvalue)

        let jlist   = listBetweenStrings "[" "]" jvalue Json.Array
        let toJsonObject = fun lst -> let map = Map.ofList lst in ReadObject (lst,map) |> Json.Object
        let jobject = listBetweenStrings "{" "}" keyValue toJsonObject

        do jvalueRef := choice [jobject
                                jlist
                                jstring
                                jnumber
                                jtrue
                                jfalse
                                jnull]

        let json = ws >>. jvalue .>> ws .>> eof

        let parseJsonString str = run json str

        let parseJsonStream stream =
            runParserOnStream json () "" stream System.Text.Encoding.UTF8

        let handleParserResult = function
            | Success (json, _, _) -> JsonResult.pass json
            | Failure (e, _, _) -> JsonResult.invalidJson e

    module Json =
        let parse s =
            if System.String.IsNullOrWhiteSpace s then
                JsonResult.noInput
            else
                Parser.parseJsonString s
                |> Parser.handleParserResult

        let parseStream (stream : #System.IO.Stream) =
            Parser.parseJsonStream stream
            |> Parser.handleParserResult

[<AutoOpen>]
module Formatting =
    open System.Globalization
    let escapeChars =
        [| '"'; '\\'; '\n'; '\r'; '\t'; '\b'; '\f'
           '\u0000'; '\u0001'; '\u0002'; '\u0003'
           '\u0004'; '\u0005'; '\u0006'; '\u0007'
           '\u000B'; '\u000E'; '\u000F'
           '\u0010'; '\u0011'; '\u0012'; '\u0013'
           '\u0014'; '\u0015'; '\u0016'; '\u0017'
           '\u0018'; '\u0019'; '\u001A'; '\u001B'
           '\u001C'; '\u001D'; '\u001E'; '\u001F' |]

    let isEscapeChar = function
        | '"' | '\\' -> true
        | c when c >= '\u0000' && c <= '\u001F' -> true
        | _ -> false

    let isEscapeCharPred = System.Predicate<_> isEscapeChar

    let escaped = function
        | '"' -> @"\"""
        | '\\' -> @"\\"
        | '\n' -> @"\n"
        | '\r' -> @"\r"
        | '\t' -> @"\t"
        | '\f' -> @"\f"
        | '\b' -> @"\b"
        | '\u0000' -> @"\u0000"
        | '\u0001' -> @"\u0001"
        | '\u0002' -> @"\u0002"
        | '\u0003' -> @"\u0003"
        | '\u0004' -> @"\u0004"
        | '\u0005' -> @"\u0005"
        | '\u0006' -> @"\u0006"
        | '\u0007' -> @"\u0007"
        | '\u000B' -> @"\u000B"
        | '\u000E' -> @"\u000E"
        | '\u000F' -> @"\u000F"
        | '\u0010' -> @"\u0010"
        | '\u0011' -> @"\u0011"
        | '\u0012' -> @"\u0012"
        | '\u0013' -> @"\u0013"
        | '\u0014' -> @"\u0014"
        | '\u0015' -> @"\u0015"
        | '\u0016' -> @"\u0016"
        | '\u0017' -> @"\u0017"
        | '\u0018' -> @"\u0018"
        | '\u0019' -> @"\u0019"
        | '\u001A' -> @"\u001A"
        | '\u001B' -> @"\u001B"
        | '\u001C' -> @"\u001C"
        | '\u001D' -> @"\u001D"
        | '\u001E' -> @"\u001E"
        | '\u001F' -> @"\u001F"
        | c -> @"\u" + (int c).ToString("X4", CultureInfo.InvariantCulture)

    type PropertyNameSpacing =
        | NoSpaceBetweenNameAndValue
        | SpaceBetweenNameAndValue

    type ElementSpacing =
        | NoSpaceBetweenElements
        | SpaceBetweenElements
        | NewLineBetweenElements of indentSpaces:int

    type JsonFormattingOptions =
      { PropertyNameSpacing: PropertyNameSpacing
        ElementSpacing: ElementSpacing }

      static member Compact =
        { PropertyNameSpacing = NoSpaceBetweenNameAndValue
          ElementSpacing = NoSpaceBetweenElements }

      static member SingleLine =
        { PropertyNameSpacing = SpaceBetweenNameAndValue
          ElementSpacing = SpaceBetweenElements }

      static member Pretty =
        { PropertyNameSpacing = SpaceBetweenNameAndValue
          ElementSpacing = NewLineBetweenElements 2 }

    module StringBuilder =
        open System.Text

        let inline append (sb: StringBuilder) (s: string) =
            sb.Append s |> ignore
        let inline appendSubstr (sb: StringBuilder) (s: string) (start: int) (count: int) =
            sb.Append (s, start, count) |> ignore
        let inline appendChar (sb: StringBuilder) (c: char) =
            sb.Append c |> ignore
        let inline appendCharRep (sb: StringBuilder) (c: char) (repeats: int) =
            sb.Append (c, repeats) |> ignore

        let writeString (sb:System.Text.StringBuilder) (cs:string) =
            let rec escapeState index =
                append sb (escaped cs.[index])
                let nextIndex = index + 1
                if nextIndex < cs.Length then
                    if isEscapeChar cs.[nextIndex] |> not then
                        coreState nextIndex
                    else
                        escapeState nextIndex
            and coreState index =
                let nextEscapeIndex = cs.IndexOfAny(escapeChars, index)
                if nextEscapeIndex = -1 then
                    appendSubstr sb cs index (cs.Length - index)
                else
                    appendSubstr sb cs index (nextEscapeIndex - index)
                    escapeState nextEscapeIndex
            coreState 0

        let appendSpaceBetweenElements sb { ElementSpacing = es } level =
            match es, level with
            | NoSpaceBetweenElements, _ -> ()
            | SpaceBetweenElements, _ -> appendChar sb ' '
            | NewLineBetweenElements _, 0 -> appendChar sb '\n'
            | NewLineBetweenElements s, l -> appendChar sb '\n'; appendCharRep sb ' ' (s * l |> int)

        let addNameSeparator sb { PropertyNameSpacing = pns } =
            match pns with
            | NoSpaceBetweenNameAndValue -> appendChar sb ':'
            | SpaceBetweenNameAndValue -> append sb ": "

        let rec formatJson sb options level = function
            | Json.Object (WriteObject props)
            | Json.Object (ReadObject (props,_)) -> formatObject sb options level props
            | Json.Array elems -> formatArray sb options level elems
            | Json.String s -> formatString sb s
            | Json.Number x -> formatNumber sb x
            | Json.True -> append sb "true"
            | Json.False -> append sb "false"
            | Json.Null -> append sb "null"

        and formatObject sb options level props =
            match props with
            | [] ->
              appendChar sb '{'
              appendChar sb '}'
            | props ->
              let newLevel = level + 1
              appendChar sb '{'
              appendSpaceBetweenElements sb options newLevel
              joinObject sb options newLevel props
              appendSpaceBetweenElements sb options level
              appendChar sb '}'

        and joinObject sb options level values =
            match values with
            | [] -> ()
            | [v] -> formatProperty sb options level v
            | v :: vs ->
                joinObject sb options level vs
                appendChar sb ','
                appendSpaceBetweenElements sb options level
                formatProperty sb options level v

        and formatProperty sb options level (k,v) =
            formatString sb k
            addNameSeparator sb options
            formatJson sb options level v

        and formatArray  sb options level elems =
            match elems with
            | [] ->
              appendChar sb '['
              appendChar sb ']'
            | elems ->
              let newLevel = level + 1
              appendChar sb '['
              appendSpaceBetweenElements sb options newLevel
              joinArray sb options newLevel elems
              appendSpaceBetweenElements sb options level
              appendChar sb ']'

        and joinArray sb options level values =
            match values with
            | [] -> ()
            | [v] -> formatJson sb options level v
            | v :: vs ->
                formatJson sb options level v
                appendChar sb ','
                appendSpaceBetweenElements sb options level
                joinArray sb options level vs

        and formatString sb str =
            appendChar sb '"'
            writeString sb str
            appendChar sb '"'

        and formatNumber sb n =
            append sb n

    [<RequireQualifiedAccess>]
    module Json =
        let formatWith options json =
            let sb = System.Text.StringBuilder ()
            StringBuilder.formatJson sb options 0 json
            sb.ToString()

        let format json =
            formatWith JsonFormattingOptions.Compact json

[<AutoOpen>]
module Serialization =
    open System.Globalization
    module Json =
        module Encode =
            let buildWith (builder: ObjectBuilder<'a>) (a: 'a): Json =
                builder a JsonObject.empty
                |> JsonObject.toJson

            let inline buildWithInline (builder: ObjectBuilder<'a>) (a: 'a): Json =
                builder a JsonObject.empty
                |> JsonObject.toJson

            let required (encode : JsonEncoder<'a>) k a jObj =
                JsonObject.add k (encode a) jObj

            let optional encode k aO jObj =
                match aO with
                | Some a -> required encode k a jObj
                | None -> jObj

            let inline ifNotEqual x encode k a jObj =
                if x = a then jObj
                else required encode k a jObj

            let conditional pred encode k a jObj =
                if pred a then required encode k a jObj
                else jObj

            let requiredMixin (build: ObjectBuilder<'a>) a jObj =
                build a jObj

            let optionalMixin (build: ObjectBuilder<'a>) aO jObj =
                match aO with
                | Some a -> build a jObj
                | None -> jObj

            let ref (): JsonEncoder<'a> ref * JsonEncoder<'a> =
                let innerRef = ref (Unchecked.defaultof<JsonEncoder<'a>>)
                innerRef, (fun a -> (!innerRef) a)

            let lazily (lazyEncode: Lazy<JsonEncoder<'a>>): JsonEncoder<'a> =
                fun a -> lazyEncode.Force() a

            let delay (u2aD: unit -> JsonEncoder<'a>): JsonEncoder<'a> =
                let lazified = lazy (u2aD ())
                lazily lazified

            let delayWith (x2aD: 'x -> JsonEncoder<'a>) (x: 'x): JsonEncoder<'a> =
                let lazified = lazy (x2aD x)
                lazily lazified

            let json json: Json = json

            let jsonObject (jObj: JsonObject): Json =
                JsonObject.toJson jObj

            let jsonObjectWith (encode: ObjectBuilder<'a>) (a: 'a): Json =
                buildWith encode a

            let propertyList (ps: (string * Json) list): Json =
                JsonObject.ofPropertyList ps
                |> jsonObject

            let propertyListWith (encode: JsonEncoder<'a>) (ps: (string * 'a) list): Json =
                JsonObject.ofPropertyListWith encode ps
                |> jsonObject

            let list (els: Json list): Json =
                Json.Array els

            let listWith (encode: JsonEncoder<'a>) (els: 'a list): Json =
                List.map encode els
                |> list

            let ilist (els: IList<Json>): Json =
                Json.Array (List.ofSeq els)

            let ilistWith (encode: JsonEncoder<'a>) (els: IList<'a>): Json =
                els
                |> Seq.map encode
                |> List.ofSeq
                |> Json.Array

            let array (els: Json array): Json =
                List.ofArray els
                |> list

            let arrayWith (encode: JsonEncoder<'a>) (els: 'a array): Json =
                Array.map encode els
                |> array

            let option (jO: Json option): Json =
                Option.defaultValue Null jO

            let optionWith (encode: JsonEncoder<'a>) (aO: 'a option): Json =
                Option.map encode aO
                |> option

            let result (jR : Result<Json, Json>) : Json =
                match jR with
                | Ok(a) -> a
                | Error(b) -> b

            let resultWith (encodeA: JsonEncoder<'a>) (encodeB: JsonEncoder<'b>) (abR: Result<'a, 'b>): Json =
                abR
                |> Result.map encodeA
                |> Result.mapError encodeB
                |> result

            // let set (els: Set<Json>): Json =
            //     Set.toList els
            //     |> list

            let setWith (encode: JsonEncoder<'a>) (els: Set<'a>): Json =
                Set.toList els
                |> listWith encode

            let map (m: Map<string, Json>): Json =
                JsonObject.ofMap m
                |> jsonObject

            let mapWith (encode: JsonEncoder<'a>) (m: Map<string, 'a>): Json =
                JsonObject.ofMapWith encode m
                |> jsonObject

            let readDictWith (encode: JsonEncoder<'a>) (m: IReadOnlyDictionary<string, 'a>): Json =
                JsonObject.ofReadDictWith encode m
                |> jsonObject

            let readDictWithCustomKey (toString: 'k -> string) (encode: JsonEncoder<'a>) (m: IReadOnlyDictionary<'k, 'a>): Json =
                JsonObject.ofReadDictWithCustomKey toString encode m
                |> jsonObject

            let mapWithCustomKey (toString: 'a -> string) (encode: JsonEncoder<'b>) (m: Map<'a, 'b>): Json =
                JsonObject.ofMapWithCustomKey toString encode m
                |> jsonObject

            let tuple2 (encodeA: JsonEncoder<'a>) (encodeB: JsonEncoder<'b>) ((a : 'a), (b : 'b)): Json =
                list
                    [ encodeA a
                      encodeB b ]

            let tuple3 (encodeA: JsonEncoder<'a>) (encodeB: JsonEncoder<'b>) (encodeC: JsonEncoder<'c>) ((a : 'a), (b : 'b), (c: 'c)): Json =
                list
                    [ encodeA a
                      encodeB b
                      encodeC c ]

            let tuple4 (encodeA: JsonEncoder<'a>) (encodeB: JsonEncoder<'b>) (encodeC: JsonEncoder<'c>) (encodeD: JsonEncoder<'d>) ((a : 'a), (b : 'b), (c: 'c), (d: 'd)): Json =
                list
                    [ encodeA a
                      encodeB b
                      encodeC c
                      encodeD d ]

            let tuple5 (encodeA: JsonEncoder<'a>) (encodeB: JsonEncoder<'b>) (encodeC: JsonEncoder<'c>) (encodeD: JsonEncoder<'d>) (encodeE: JsonEncoder<'e>) ((a : 'a), (b : 'b), (c: 'c), (d: 'd), (e: 'e)): Json =
                list
                    [ encodeA a
                      encodeB b
                      encodeC c
                      encodeD d
                      encodeE e ]

            let tuple6 (encodeA: JsonEncoder<'a>) (encodeB: JsonEncoder<'b>) (encodeC: JsonEncoder<'c>) (encodeD: JsonEncoder<'d>) (encodeE: JsonEncoder<'e>) (encodeF: JsonEncoder<'f>) ((a : 'a), (b : 'b), (c: 'c), (d: 'd), (e: 'e), (f: 'f)): Json =
                list
                    [ encodeA a
                      encodeB b
                      encodeC c
                      encodeD d
                      encodeE e
                      encodeF f ]

            let tuple7 (encodeA: JsonEncoder<'a>) (encodeB: JsonEncoder<'b>) (encodeC: JsonEncoder<'c>) (encodeD: JsonEncoder<'d>) (encodeE: JsonEncoder<'e>) (encodeF: JsonEncoder<'f>) (encodeG: JsonEncoder<'g>) ((a : 'a), (b : 'b), (c: 'c), (d: 'd), (e: 'e), (f: 'f), (g: 'g)): Json =
                list
                    [ encodeA a
                      encodeB b
                      encodeC c
                      encodeD d
                      encodeE e
                      encodeF f
                      encodeG g ]

            let string (str: string): Json =
                Json.String str

            let number (n: string): Json =
                Json.Number n

            let int16 (n: int16): Json =
                n.ToString(CultureInfo.InvariantCulture)
                |> number

            let int (n: int): Json =
                n.ToString(CultureInfo.InvariantCulture)
                |> number

            let int64 (n: int64): Json =
                n.ToString(CultureInfo.InvariantCulture)
                |> number

            let uint16 (n: uint16): Json =
                n.ToString(CultureInfo.InvariantCulture)
                |> number

            let uint32 (n: uint32): Json =
                n.ToString(CultureInfo.InvariantCulture)
                |> number

            let uint64 (n: uint64): Json =
                n.ToString(CultureInfo.InvariantCulture)
                |> number

            let single (n: single): Json =
                n.ToString("R", CultureInfo.InvariantCulture)
                |> number

            let float (n: float): Json =
                n.ToString("G17", CultureInfo.InvariantCulture)
                |> number

            let decimal (n: decimal): Json =
                n.ToString(CultureInfo.InvariantCulture)
                |> number

            let bigint (n: bigint): Json =
                n.ToString("R", CultureInfo.InvariantCulture)
                |> number

            let dateTime (dt: System.DateTime): Json =
                dt.ToUniversalTime().ToString("o", CultureInfo.InvariantCulture)
                |> string

            let dateTimeOffset (dt: System.DateTimeOffset): Json =
                dt.ToString("o", CultureInfo.InvariantCulture)
                |> string

            let guid (g: System.Guid): Json =
                g.ToString("N")
                |> string

            let bytes (bs: byte array): Json =
                System.Convert.ToBase64String bs
                |> string

            let bool (b: bool): Json =
                if b then Json.True else Json.False

            let unit () : Json =
                Json.Null

            let intList = listWith int
            let intArray = arrayWith int
            let intSet = setWith int
            let stringList = listWith string
            let stringArray = arrayWith string
            let stringSet = setWith string
            let stringMap = mapWith string

        module Decode =
            open NodaTime.Text
            open NodaTime
            open Operators

            let always a : Decoder<'x,'a> =
                Decoder.alwaysPass a

            let json : Decoder<Json,Json> = JsonResult.pass

            let unit : Decoder<Json,unit> =
                do ()
                function
                | Json.Null -> JsonResult.pass ()
                | json -> JsonResult.typeMismatch JsonMemberType.Null json

            let jsonObject : Decoder<Json,JsonObject> =
                (do ())
                function
                | Json.Object o -> JsonResult.pass o
                | json -> JsonResult.typeMismatch JsonMemberType.Object json

            let jsonObjectWith (decode: Decoder<JsonObject,'a>) : Decoder<Json,'a> =
                jsonObject >=> decode

            let required (decode: Decoder<Json,'a>) (k: string) : Decoder<JsonObject,'a> =
                let binder = Decoder.withPropertyTag k decode
                fun jObj ->
                    JsonObject.find k jObj
                    |> JsonResult.bind binder

            let inline requiredInline (decode: Decoder<Json,'a>) k : Decoder<JsonObject,'a> =
                fun jObj ->
                    JsonObject.find k jObj
                    |> JsonResult.bind (Decoder.withPropertyTagInline k decode)

            let optional (decode: Decoder<Json,'a>) (k: string) : Decoder<JsonObject,'a option> =
                fun jObj ->
                    match JsonObject.tryFind k jObj with
                    | Some json ->
                        match Decoder.withPropertyTag k decode json with
                        | JPass x -> JsonResult.pass (Some x)
                        | JFail f -> JsonResult.fail f
                    | None -> JsonResult.pass None

            let inline optionalInline (decode: Decoder<Json,'a>) (k: string) : Decoder<JsonObject,'a option> =
                fun jObj ->
                    match JsonObject.tryFind k jObj with
                    | Some json ->
                        match Decoder.withPropertyTagInline k decode json with
                        | JPass x -> JsonResult.pass (Some x)
                        | JFail f -> JsonResult.fail f
                    | None -> JsonResult.pass None

            let withDefault (def: 's) : Decoder<'s option, 's> =
                fun sO -> JPass (Option.defaultValue def sO)

            let optionalList (decode: Decoder<Json,'a list>) (k: string) : Decoder<JsonObject,'a list> =
                optional decode k >=> withDefault []

            let optionalMap (decode: Decoder<Json,Map<'a,'b>>) (k:string) : Decoder<JsonObject,Map<'a,'b>> =
                optional decode k >=> withDefault Map.empty

            let optionalArray (decode: Decoder<Json,'a array>) (k: string): Decoder<JsonObject,'a array> =
                optional decode k >=> withDefault [||]

            let requiredMixin (decode: Decoder<Json,'a>) : Decoder<JsonObject,'a> =
                fun jObj ->
                    Encode.jsonObject jObj
                    |> decode

            let ref (): Decoder<'s,'a> ref * Decoder<'s,'a> =
                let innerRef = ref (Unchecked.defaultof<Decoder<'s,'a>>)
                innerRef, (fun s -> (!innerRef) s)

            let lazily (lazyDecode: Lazy<Decoder<'s,'a>>): Decoder<'s,'a> =
                fun s ->
                    lazyDecode.Force() s

            let delay (u2aD: unit -> Decoder<'s,'a>): Decoder<'s,'a> =
                let lazified = lazy (u2aD ())
                lazily lazified

            let delayWith (x2aD: 'x -> Decoder<'s,'a>) (x: 'x): Decoder<'s,'a> =
                let lazified = lazy (x2aD x)
                lazily lazified

            let either (decodeA: Decoder<'s,'a>) (decodeB: Decoder<'s,'a>) =
                fun s ->
                    match Decoder.withChoiceTag 0u decodeA s with
                    | (JPass _) as goodResult -> goodResult
                    | JFail errs1 ->
                        match Decoder.withChoiceTag 1u decodeB s with
                        | (JPass _) as goodResult -> goodResult
                        | JFail errs2 ->
                            JsonResult.fail (JsonFailure.mappend errs1 errs2)

            let oneOf (decoders: Decoder<'s,'a> list) : Decoder<'s,'a> =
                let rec failing decode decoders (aggErrs: JsonFailure list) i (s: 's) =
                    match Decoder.withChoiceTag i decode s with
                    | (JPass _) as goodResult -> goodResult
                    | JFail errs ->
                        let nextAggErrs = errs::aggErrs
                        match decoders with
                        | [] -> JsonResult.fails nextAggErrs
                        | nextDecode::nextDecoders ->
                            failing nextDecode nextDecoders nextAggErrs (i + 1u) s
                match decoders with
                | [] -> fun s -> JsonResult.deserializationError (exn "No decoders provided to oneOf")
                | [decode] -> decode
                | initDecode::initDecoders -> failing initDecode initDecoders [] 0u

            let optionalMixin (decode: Decoder<Json,'a>) : Decoder<JsonObject,'a option> =
                either (requiredMixin decode >-> Some) (always None)

            let assertThat (pred: 's -> bool) (failMsg: string) : Decoder<'s,'s> =
                fun s ->
                    if pred s then
                        JsonResult.pass s
                    else
                        JsonResult.raise (exn failMsg)

            let propertyList : Decoder<Json,(string * Json) list> =
                JsonObject.toPropertyList <!> jsonObject

            let propertyListWithQuick (decode: Decoder<Json,'a>) : Decoder<Json,(string * 'a) list> =
                jsonObject >=> JsonObject.toPropertyListWithQuick decode

            let propertyListWith (decode: Decoder<Json,'a>) : Decoder<Json,(string * 'a) list> =
                jsonObject >=> JsonObject.toPropertyListWith decode

            let propertyListWithCustomKeyQuick (parse: string -> JsonResult<'k>) (decode: Decoder<Json,'v>) : Decoder<Json,('k * 'v) list> =
                jsonObject >=> JsonObject.toPropertyListWithCustomKeyQuick parse decode

            let propertyListWithCustomKey (parse: string -> JsonResult<'k>) (decode: Decoder<Json,'v>) : Decoder<Json,('k * 'v) list> =
                jsonObject >=> JsonObject.toPropertyListWithCustomKey parse decode

            let list : Decoder<Json,Json list> =
                do ()
                function
                | Json.Array a -> JsonResult.pass a
                | json -> JsonResult.typeMismatch JsonMemberType.Array json

            let array : Decoder<Json,Json array>=
                Array.ofList <!> list

            let arrayWithQuick (decode: Decoder<Json,'a>) : Decoder<Json,'a[]> =
                let idλ = (do ()); fun (x: JsonFailure list) -> x
                let singletonArrayλ = (do ()); fun (x: 'a) -> [|x|]
                let rec goodPath (agg: 'a[]) idx lastX xs =
                    match Decoder.withIndexTag idx decode lastX with
                    | JPass x ->
                        agg.[int idx] <- x
                        match xs with
                        | [] -> JsonResult.pass agg
                        | nextX::nextXs -> goodPath agg (idx + 1u) nextX nextXs
                    | JFail errs ->
                        JsonResult.fail errs
                list >=> function
                | [] -> JsonResult.pass [||]
                | [x] -> decode x |> JsonResult.map singletonArrayλ
                | x::xs -> goodPath (Array.zeroCreate (List.length xs + 1)) 0u x xs

            let arrayWith (decode: Decoder<Json,'a>) : Decoder<Json,'a[]> =
                let idλ = (do ()); fun (x: JsonFailure list) -> x
                let singletonArrayλ = (do ()); fun (x: 'a) -> [|x|]
                let rec goodPath (agg: 'a[]) idx lastX xs =
                    match Decoder.withIndexTag idx decode lastX with
                    | JPass x ->
                        agg.[int idx] <- x
                        match xs with
                        | [] -> JsonResult.pass agg
                        | nextX::nextXs -> goodPath agg (idx + 1u) nextX nextXs
                    | JFail errs ->
                        badPathTransfer [errs] idx xs
                and badPath (aggErrs: JsonFailure list) idx lastX xs =
                    match Decoder.withIndexTag idx decode lastX with
                    | JFail errs ->
                        badPathTransfer (errs::aggErrs) idx xs
                    | _ ->
                        badPathTransfer aggErrs idx xs
                and badPathTransfer aggErrs idx xs =
                    match xs with
                    | [] -> JsonResult.fails aggErrs
                    | nextX::nextXs -> badPath aggErrs (idx + 1u) nextX nextXs
                list >=> function
                | [] -> JsonResult.pass [||]
                | [x] -> decode x |> JsonResult.map singletonArrayλ
                | x::xs -> goodPath (Array.zeroCreate (List.length xs + 1)) 0u x xs

            let listWithQuick (decode: Decoder<Json,'a>) : Decoder<Json,'a list> =
                Array.toList <!> arrayWithQuick decode

            let listWith (decode: Decoder<Json,'a>) : Decoder<Json,'a list> =
                Array.toList <!> arrayWith decode

            let optionWith (decode: Decoder<Json,'a>) : Decoder<Json,'a option> =
                let decode =
                    oneOf
                        [ unit >=> always None
                          decode >-> Some ]
                fun json -> decode json

            let option : Decoder<Json,Json option> =
                optionWith json

            // let set (json: Json) =
            //     Set.ofList <!> list

            let setWithQuick (decode: Decoder<Json,'a>) : Decoder<Json,Set<'a>> =
                Set.ofArray <!> arrayWithQuick decode

            let setWith (decode: Decoder<Json,'a>) : Decoder<Json,Set<'a>> =
                Set.ofArray <!> arrayWith decode

            let resultWith (decodeA: Decoder<Json, 'a>) (decodeB : Decoder<Json, 'b>) : Decoder<Json, Result<'a, 'b>> =
                fun s ->
                    match Decoder.withChoiceTag 0u decodeA s with
                    | (JPass _) as goodResult -> JsonResult.map Ok goodResult
                    | JFail errs1 ->
                        match Decoder.withChoiceTag 1u decodeB s with
                        | (JPass _) as goodResult -> JsonResult.map Error goodResult
                        | JFail errs2 ->
                            JsonResult.fail (JsonFailure.mappend errs1 errs2)

            let map : Decoder<Json,Map<string,Json>> =
                JsonObject.toMap <!> jsonObject

            let mapWithQuick (decode: Decoder<Json,'a>) : Decoder<Json,Map<string,'a>> =
                Map.ofList <!> (jsonObject >=> JsonObject.toPropertyListWithQuick decode)

            let mapWith (decode: Decoder<Json,'a>) : Decoder<Json,Map<string,'a>> =
                Map.ofList <!> (jsonObject >=> JsonObject.toPropertyListWith decode)

            let mapWithCustomKeyQuick (parse: string -> JsonResult<'k>) (decode: Decoder<Json,'v>) : Decoder<Json,Map<'k,'v>> =
                Map.ofList <!> (jsonObject >=> JsonObject.toPropertyListWithCustomKeyQuick parse decode)

            let mapWithCustomKey (parse: string -> JsonResult<'k>) (decode: Decoder<Json,'v>) : Decoder<Json,Map<'k,'v>> =
                Map.ofList <!> (jsonObject >=> JsonObject.toPropertyListWithCustomKey parse decode)

            let inline listToTuple2 lst =
                match lst with
                | [a;b] -> JsonResult.pass (a, b)
                | [_] -> JsonResult.deserializationError (exn "2-Tuple has only one element")
                | [] -> JsonResult.deserializationError (exn "2-Tuple has zero elements")
                | _ -> JsonResult.deserializationError (exn "2-Tuple has too many elements")

            let tuple2 : Decoder<Json,Json * Json> =
                list >=> listToTuple2

            let inline mkTuple2 a b = (a, b)
            let inline tuple2WithTaggedDecoders f decodeA decodeB =
                let tagA = Decoder.withIndexTag 0u decodeA
                let tagB = Decoder.withIndexTag 1u decodeB
                f mkTuple2 tagA tagB

            let inline jsonTuple2ToTuple2Quick mkTuple (decodeA: Decoder<Json,'a>) (decodeB: Decoder<Json,'b>) =
                fun (a, b) ->
                    JsonResult.map mkTuple (decodeA a)
                    |> JsonResult.applyDelayedWithPolicy StopOnFirstError decodeB b

            let inline jsonTuple2ToTuple2 mkTuple (decodeA: Decoder<Json,'a>) (decodeB: Decoder<Json,'b>) =
                fun (a, b) ->
                    JsonResult.map mkTuple (decodeA a)
                    |> JsonResult.applyDelayedWithPolicy ContinueOnError decodeB b

            let tuple2WithQuick (decodeA: Decoder<Json,'a>) (decodeB: Decoder<Json,'b>) : Decoder<Json,'a * 'b> =
                tuple2 >=> tuple2WithTaggedDecoders jsonTuple2ToTuple2Quick decodeA decodeB

            let tuple2With (decodeA: Decoder<Json,'a>) (decodeB: Decoder<Json,'b>) : Decoder<Json,'a * 'b> =
                tuple2 >=> tuple2WithTaggedDecoders jsonTuple2ToTuple2 decodeA decodeB

            let inline listToTuple3 lst =
                match lst with
                | [a;b;c] -> JsonResult.pass (a, b, c)
                | x when List.length x > 3 -> JsonResult.deserializationError (exn "3-Tuple has excess elements")
                | _ -> JsonResult.deserializationError (exn "3-Tuple has insufficient elements")

            let tuple3 =
                list >=> listToTuple3

            let inline mkTuple3 a b c = (a, b, c)
            let inline tuple3WithTaggedDecoders f decodeA decodeB decodeC =
                let tagA = Decoder.withIndexTag 0u decodeA
                let tagB = Decoder.withIndexTag 1u decodeB
                let tagC = Decoder.withIndexTag 2u decodeC
                f mkTuple3 tagA tagB tagC

            let inline jsonTuple3ToTuple3Quick mkTuple (decodeA: Decoder<Json,'a>) (decodeB: Decoder<Json,'b>) (decodeC: Decoder<Json,'c>) =
                fun (a, b, c) ->
                    JsonResult.map mkTuple (decodeA a)
                    |> JsonResult.applyDelayedWithPolicy StopOnFirstError decodeB b
                    |> JsonResult.applyDelayedWithPolicy StopOnFirstError decodeC c

            let inline jsonTuple3ToTuple3 mkTuple (decodeA: Decoder<Json,'a>) (decodeB: Decoder<Json,'b>) (decodeC: Decoder<Json,'c>) =
                fun (a, b, c) ->
                    JsonResult.map mkTuple (decodeA a)
                    |> JsonResult.applyDelayedWithPolicy ContinueOnError decodeB b
                    |> JsonResult.applyDelayedWithPolicy ContinueOnError decodeC c

            let tuple3WithQuick (decodeA: Decoder<Json,'a>) (decodeB: Decoder<Json,'b>) (decodeC: Decoder<Json,'c>): Decoder<Json,'a * 'b * 'c> =
                tuple3 >=> tuple3WithTaggedDecoders jsonTuple3ToTuple3Quick decodeA decodeB decodeC

            let tuple3With (decodeA: Decoder<Json,'a>) (decodeB: Decoder<Json,'b>) (decodeC: Decoder<Json,'c>): Decoder<Json,'a * 'b * 'c> =
                tuple3 >=> tuple3WithTaggedDecoders jsonTuple3ToTuple3 decodeA decodeB decodeC

            let inline listToTuple4 lst =
                match lst with
                | [a;b;c;d] -> JsonResult.pass (a, b, c, d)
                | x when List.length x > 4 -> JsonResult.deserializationError (exn "4-Tuple has excess elements")
                | _ -> JsonResult.deserializationError (exn "4-Tuple has insufficient elements")

            let tuple4 =
                list >=> listToTuple4

            let inline mkTuple4 a b c d = (a, b, c, d)
            let inline tuple4WithTaggedDecoders f decodeA decodeB decodeC decodeD =
                let tagA = Decoder.withIndexTag 0u decodeA
                let tagB = Decoder.withIndexTag 1u decodeB
                let tagC = Decoder.withIndexTag 2u decodeC
                let tagD = Decoder.withIndexTag 3u decodeD
                f mkTuple4 tagA tagB tagC tagD

            let inline jsonTuple4ToTuple4Quick mkTuple (decodeA: Decoder<Json,'a>) (decodeB: Decoder<Json,'b>) (decodeC: Decoder<Json,'c>) (decodeD: Decoder<Json,'d>) =
                fun (a, b, c, d) ->
                    JsonResult.map mkTuple (decodeA a)
                    |> JsonResult.applyDelayedWithPolicy StopOnFirstError decodeB b
                    |> JsonResult.applyDelayedWithPolicy StopOnFirstError decodeC c
                    |> JsonResult.applyDelayedWithPolicy StopOnFirstError decodeD d

            let inline jsonTuple4ToTuple4 mkTuple (decodeA: Decoder<Json,'a>) (decodeB: Decoder<Json,'b>) (decodeC: Decoder<Json,'c>) (decodeD: Decoder<Json,'d>) =
                fun (a, b, c, d) ->
                    JsonResult.map mkTuple (decodeA a)
                    |> JsonResult.applyDelayedWithPolicy ContinueOnError decodeB b
                    |> JsonResult.applyDelayedWithPolicy ContinueOnError decodeC c
                    |> JsonResult.applyDelayedWithPolicy ContinueOnError decodeD d

            let tuple4WithQuick (decodeA: Decoder<Json,'a>) (decodeB: Decoder<Json,'b>) (decodeC: Decoder<Json,'c>) (decodeD: Decoder<Json,'d>) =
                tuple4 >=> tuple4WithTaggedDecoders jsonTuple4ToTuple4Quick decodeA decodeB decodeC decodeD

            let tuple4With (decodeA: Decoder<Json,'a>) (decodeB: Decoder<Json,'b>) (decodeC: Decoder<Json,'c>) (decodeD: Decoder<Json,'d>) =
                tuple4 >=> tuple4WithTaggedDecoders jsonTuple4ToTuple4 decodeA decodeB decodeC decodeD

            let inline listToTuple5 lst =
                match lst with
                | [a;b;c;d;e] -> JsonResult.pass (a, b, c, d, e)
                | x when List.length x > 5 -> JsonResult.deserializationError (exn "5-Tuple has excess elements")
                | _ -> JsonResult.deserializationError (exn "5-Tuple has insufficient elements")

            let tuple5 =
                list >=> listToTuple5

            let inline mkTuple5 a b c d e = (a, b, c, d, e)
            let inline tuple5WithTaggedDecoders f decodeA decodeB decodeC decodeD decodeE =
                let tagA = Decoder.withIndexTag 0u decodeA
                let tagB = Decoder.withIndexTag 1u decodeB
                let tagC = Decoder.withIndexTag 2u decodeC
                let tagD = Decoder.withIndexTag 3u decodeD
                let tagE = Decoder.withIndexTag 4u decodeE
                f mkTuple5 tagA tagB tagC tagD tagE

            let inline jsonTuple5ToTuple5Quick mkTuple (decodeA: Decoder<Json,'a>) (decodeB: Decoder<Json,'b>) (decodeC: Decoder<Json,'c>) (decodeD: Decoder<Json,'d>) (decodeE: Decoder<Json,'e>) =
                fun (a, b, c, d, e) ->
                    JsonResult.map mkTuple (decodeA a)
                    |> JsonResult.applyDelayedWithPolicy StopOnFirstError decodeB b
                    |> JsonResult.applyDelayedWithPolicy StopOnFirstError decodeC c
                    |> JsonResult.applyDelayedWithPolicy StopOnFirstError decodeD d
                    |> JsonResult.applyDelayedWithPolicy StopOnFirstError decodeE e

            let inline jsonTuple5ToTuple5 mkTuple (decodeA: Decoder<Json,'a>) (decodeB: Decoder<Json,'b>) (decodeC: Decoder<Json,'c>) (decodeD: Decoder<Json,'d>) (decodeE: Decoder<Json,'e>) =
                fun (a, b, c, d, e) ->
                    JsonResult.map mkTuple (decodeA a)
                    |> JsonResult.applyDelayedWithPolicy ContinueOnError decodeB b
                    |> JsonResult.applyDelayedWithPolicy ContinueOnError decodeC c
                    |> JsonResult.applyDelayedWithPolicy ContinueOnError decodeD d
                    |> JsonResult.applyDelayedWithPolicy ContinueOnError decodeE e

            let tuple5WithQuick (decodeA: Decoder<Json,'a>) (decodeB: Decoder<Json,'b>) (decodeC: Decoder<Json,'c>) (decodeD: Decoder<Json,'d>) (decodeE: Decoder<Json,'e>) =
                tuple5 >=> tuple5WithTaggedDecoders jsonTuple5ToTuple5Quick decodeA decodeB decodeC decodeD decodeE

            let tuple5With (decodeA: Decoder<Json,'a>) (decodeB: Decoder<Json,'b>) (decodeC: Decoder<Json,'c>) (decodeD: Decoder<Json,'d>) (decodeE: Decoder<Json,'e>) =
                tuple5 >=> tuple5WithTaggedDecoders jsonTuple5ToTuple5 decodeA decodeB decodeC decodeD decodeE

            let listToTuple6 lst =
                match lst with
                | [a;b;c;d;e;f] -> JsonResult.pass (a, b, c, d, e, f)
                | x when List.length x > 6 -> JsonResult.deserializationError (exn "6-Tuple has excess elements")
                | _ -> JsonResult.deserializationError (exn "6-Tuple has insufficient elements")

            let tuple6 =
                list >=> listToTuple6

            let inline mkTuple6 a b c d e f = (a, b, c, d, e, f)
            let inline tuple6WithTaggedDecoders f decodeA decodeB decodeC decodeD decodeE decodeF =
                let tagA = Decoder.withIndexTag 0u decodeA
                let tagB = Decoder.withIndexTag 1u decodeB
                let tagC = Decoder.withIndexTag 2u decodeC
                let tagD = Decoder.withIndexTag 3u decodeD
                let tagE = Decoder.withIndexTag 4u decodeE
                let tagF = Decoder.withIndexTag 5u decodeF
                f mkTuple6 tagA tagB tagC tagD tagE tagF

            let inline jsonTuple6ToTuple6Quick mkTuple (decodeA: Decoder<Json,'a>) (decodeB: Decoder<Json,'b>) (decodeC: Decoder<Json,'c>) (decodeD: Decoder<Json,'d>) (decodeE: Decoder<Json,'e>) (decodeF: Decoder<Json,'f>) =
                fun (a, b, c, d, e, f) ->
                    JsonResult.map mkTuple (decodeA a)
                    |> JsonResult.applyDelayedWithPolicy StopOnFirstError decodeB b
                    |> JsonResult.applyDelayedWithPolicy StopOnFirstError decodeC c
                    |> JsonResult.applyDelayedWithPolicy StopOnFirstError decodeD d
                    |> JsonResult.applyDelayedWithPolicy StopOnFirstError decodeE e
                    |> JsonResult.applyDelayedWithPolicy StopOnFirstError decodeF f

            let inline jsonTuple6ToTuple6 mkTuple (decodeA: Decoder<Json,'a>) (decodeB: Decoder<Json,'b>) (decodeC: Decoder<Json,'c>) (decodeD: Decoder<Json,'d>) (decodeE: Decoder<Json,'e>) (decodeF: Decoder<Json,'f>) =
                fun (a, b, c, d, e, f) ->
                    JsonResult.map mkTuple (decodeA a)
                    |> JsonResult.applyDelayedWithPolicy ContinueOnError decodeB b
                    |> JsonResult.applyDelayedWithPolicy ContinueOnError decodeC c
                    |> JsonResult.applyDelayedWithPolicy ContinueOnError decodeD d
                    |> JsonResult.applyDelayedWithPolicy ContinueOnError decodeE e
                    |> JsonResult.applyDelayedWithPolicy ContinueOnError decodeF f

            let tuple6WithQuick (decodeA: Decoder<Json,'a>) (decodeB: Decoder<Json,'b>) (decodeC: Decoder<Json,'c>) (decodeD: Decoder<Json,'d>) (decodeE: Decoder<Json,'e>) (decodeF: Decoder<Json,'f>) =
                tuple6 >=> tuple6WithTaggedDecoders jsonTuple6ToTuple6Quick decodeA decodeB decodeC decodeD decodeE decodeF

            let tuple6With (decodeA: Decoder<Json,'a>) (decodeB: Decoder<Json,'b>) (decodeC: Decoder<Json,'c>) (decodeD: Decoder<Json,'d>) (decodeE: Decoder<Json,'e>) (decodeF: Decoder<Json,'f>) =
                tuple6 >=> tuple6WithTaggedDecoders jsonTuple6ToTuple6 decodeA decodeB decodeC decodeD decodeE decodeF

            let inline listToTuple7 lst =
                match lst with
                | [a;b;c;d;e;f;g] -> JsonResult.pass (a, b, c, d, e, f, g)
                | x when List.length x > 7 -> JsonResult.deserializationError (exn "7-Tuple has excess elements")
                | _ -> JsonResult.deserializationError (exn "7-Tuple has insufficient elements")

            let tuple7 =
                list >=> listToTuple7

            let inline mkTuple7 a b c d e f g = (a, b, c, d, e, f, g)
            let inline tuple7WithTaggedDecoders f decodeA decodeB decodeC decodeD decodeE decodeF decodeG =
                let tagA = Decoder.withIndexTag 0u decodeA
                let tagB = Decoder.withIndexTag 1u decodeB
                let tagC = Decoder.withIndexTag 2u decodeC
                let tagD = Decoder.withIndexTag 3u decodeD
                let tagE = Decoder.withIndexTag 4u decodeE
                let tagF = Decoder.withIndexTag 5u decodeF
                let tagG = Decoder.withIndexTag 6u decodeG
                f mkTuple7 tagA tagB tagC tagD tagE tagF tagG

            let inline jsonTuple7ToTuple7Quick mkTuple (decodeA: Decoder<Json,'a>) (decodeB: Decoder<Json,'b>) (decodeC: Decoder<Json,'c>) (decodeD: Decoder<Json,'d>) (decodeE: Decoder<Json,'e>) (decodeF: Decoder<Json,'f>) (decodeG: Decoder<Json,'g>) =
                fun (a, b, c, d, e, f, g) ->
                    JsonResult.map mkTuple (decodeA a)
                    |> JsonResult.applyDelayedWithPolicy StopOnFirstError decodeB b
                    |> JsonResult.applyDelayedWithPolicy StopOnFirstError decodeC c
                    |> JsonResult.applyDelayedWithPolicy StopOnFirstError decodeD d
                    |> JsonResult.applyDelayedWithPolicy StopOnFirstError decodeE e
                    |> JsonResult.applyDelayedWithPolicy StopOnFirstError decodeF f
                    |> JsonResult.applyDelayedWithPolicy StopOnFirstError decodeG g

            let inline jsonTuple7ToTuple7 mkTuple (decodeA: Decoder<Json,'a>) (decodeB: Decoder<Json,'b>) (decodeC: Decoder<Json,'c>) (decodeD: Decoder<Json,'d>) (decodeE: Decoder<Json,'e>) (decodeF: Decoder<Json,'f>) (decodeG: Decoder<Json,'g>) =
                fun (a, b, c, d, e, f, g) ->
                    JsonResult.map mkTuple (decodeA a)
                    |> JsonResult.applyDelayedWithPolicy ContinueOnError decodeB b
                    |> JsonResult.applyDelayedWithPolicy ContinueOnError decodeC c
                    |> JsonResult.applyDelayedWithPolicy ContinueOnError decodeD d
                    |> JsonResult.applyDelayedWithPolicy ContinueOnError decodeE e
                    |> JsonResult.applyDelayedWithPolicy ContinueOnError decodeF f
                    |> JsonResult.applyDelayedWithPolicy ContinueOnError decodeG g

            let tuple7WithQuick (decodeA: Decoder<Json,'a>) (decodeB: Decoder<Json,'b>) (decodeC: Decoder<Json,'c>) (decodeD: Decoder<Json,'d>) (decodeE: Decoder<Json,'e>) (decodeF: Decoder<Json,'f>) (decodeG: Decoder<Json,'g>) =
                tuple7 >=> tuple7WithTaggedDecoders jsonTuple7ToTuple7Quick decodeA decodeB decodeC decodeD decodeE decodeF decodeG

            let tuple7With (decodeA: Decoder<Json,'a>) (decodeB: Decoder<Json,'b>) (decodeC: Decoder<Json,'c>) (decodeD: Decoder<Json,'d>) (decodeE: Decoder<Json,'e>) (decodeF: Decoder<Json,'f>) (decodeG: Decoder<Json,'g>) =
                tuple7 >=> tuple7WithTaggedDecoders jsonTuple7ToTuple7 decodeA decodeB decodeC decodeD decodeE decodeF decodeG

            let number =
                do ()
                function
                | Json.Number n -> JsonResult.pass n
                | json -> JsonResult.typeMismatch JsonMemberType.Number json

            let int16 =
                number >=> Decoder.fromThrowingConverter (fun s -> System.Int16.Parse(s, CultureInfo.InvariantCulture))

            let int =
                number >=> Decoder.fromThrowingConverter (fun s -> System.Int32.Parse(s, CultureInfo.InvariantCulture))

            let int64 =
                number >=> Decoder.fromThrowingConverter (fun s -> System.Int64.Parse(s, CultureInfo.InvariantCulture))

            let uint16 =
                number >=> Decoder.fromThrowingConverter (fun s -> System.UInt16.Parse(s, CultureInfo.InvariantCulture))

            let uint32 =
                number >=> Decoder.fromThrowingConverter (fun s -> System.UInt32.Parse(s, CultureInfo.InvariantCulture))

            let uint64 =
                number >=> Decoder.fromThrowingConverter (fun s -> System.UInt64.Parse(s, CultureInfo.InvariantCulture))

            let single =
                number >=> Decoder.fromThrowingConverter (fun s -> System.Single.Parse(s, CultureInfo.InvariantCulture))

            let float =
                number >=> Decoder.fromThrowingConverter (fun s -> System.Double.Parse(s, CultureInfo.InvariantCulture))

            let decimal =
                number >=> Decoder.fromThrowingConverter (fun s -> System.Decimal.Parse(s, CultureInfo.InvariantCulture))

            let bigint =
                number >=> Decoder.fromThrowingConverter (fun s -> System.Numerics.BigInteger.Parse(s, CultureInfo.InvariantCulture))

            let string =
                do ()
                function
                | Json.String s -> JsonResult.pass s
                | json -> JsonResult.typeMismatch JsonMemberType.String json

            let instant =
              let parsers =
                [
                   "yyyy'-'MM'-'dd'T'HH':'mm':'ss'.'FFFFFFFFF'Z'"
                   "yyyy'-'MM'-'dd'T'HH':'mm':'ss'.'FFFFFFFF'Z'"
                   "yyyy'-'MM'-'dd'T'HH':'mm':'ss'.'FFFFFFF'Z'"
                   "yyyy'-'MM'-'dd'T'HH':'mm':'ss'.'FFFFFF'Z'"
                   "yyyy'-'MM'-'dd'T'HH':'mm':'ss'.'FFFFF'Z'"
                   "yyyy'-'MM'-'dd'T'HH':'mm':'ss'.'FFFF'Z'"
                   "yyyy'-'MM'-'dd'T'HH':'mm':'ss'.'FFF'Z'"
                   "yyyy'-'MM'-'dd'T'HH':'mm':'ss'.'F'Z'"
                   "yyyy'-'MM'-'dd'T'HH':'mm':'ss'Z'"
                ]
                |> List.map (fun pattern -> InstantPattern.Create(pattern, CultureInfo.InvariantCulture).Parse)
                |> List.map Decoder.fromNodaTime
              string >=> oneOf parsers

            let offsetDateTime =
              let parsers =
                [
                   "yyyy'-'MM'-'dd'T'HH':'mm':'ss;FFFFFFFFFo<G>"
                   "yyyy'-'MM'-'dd'T'HH':'mm':'ss;FFFFFFFFo<G>"
                   "yyyy'-'MM'-'dd'T'HH':'mm':'ss;FFFFFFFo<G>"
                   "yyyy'-'MM'-'dd'T'HH':'mm':'ss;FFFFFFo<G>"
                   "yyyy'-'MM'-'dd'T'HH':'mm':'ss;FFFFFo<G>"
                   "yyyy'-'MM'-'dd'T'HH':'mm':'ss;FFFFo<G>"
                   "yyyy'-'MM'-'dd'T'HH':'mm':'ss;FFFo<G>"
                   "yyyy'-'MM'-'dd'T'HH':'mm':'ss;FFo<G>"
                   "yyyy'-'MM'-'dd'T'HH':'mm':'ss;Fo<G>"
                   "yyyy'-'MM'-'dd'T'HH':'mm':'sso<G>"
                ]
                |> List.map (fun pattern -> OffsetDateTimePattern.Create(pattern, CultureInfo.InvariantCulture, OffsetDateTime()).Parse)
                |> List.map Decoder.fromNodaTime
              string >=> oneOf parsers

            let dateTimeOffset =
                let toDTO (odt: OffsetDateTime) = odt.ToDateTimeOffset()
                offsetDateTime |> Decoder.map toDTO

            let dateTimeParser (s: string) =
              System.DateTime.ParseExact (s, [| "s"; "r"; "o" |], CultureInfo.InvariantCulture, System.Globalization.DateTimeStyles.AdjustToUniversal)

            let dateTime =
                string >=> Decoder.fromThrowingConverter dateTimeParser

            let guid =
                string >=> Decoder.fromThrowingConverter System.Guid.Parse

            let bytes =
                string >=> Decoder.fromThrowingConverter System.Convert.FromBase64String

            let bool =
                do ()
                function
                | Json.True -> JsonResult.pass true
                | Json.False -> JsonResult.pass false
                | json -> JsonResult.typeMismatch JsonMemberType.Bool json

            let intArrayQuick = arrayWithQuick int
            let stringArrayQuick = arrayWithQuick string
            let intArray = arrayWith int
            let stringArray = arrayWith string
            let intListQuick = listWithQuick int
            let stringListQuick = listWithQuick string
            let intList = listWith int
            let stringList = listWith string
            let stringSetQuick = setWithQuick string
            let stringSet = setWith string

        let deserializeWith (decode: Decoder<Json,'a>) (str: string) =
            Json.parse str
            |> JsonResult.bind decode

        let deserializeObjectWith (decode: Decoder<JsonObject,'a>) (str: string) =
            Json.parse str
            |> JsonResult.bind (Decode.jsonObject)
            |> JsonResult.bind decode

        let serializeWith (encode: JsonEncoder<'a>) (options: JsonFormattingOptions) (a: 'a) =
            encode a
            |> Json.formatWith options

        let serializeObjectWith (builder: ObjectBuilder<'a>) (options: JsonFormattingOptions) (a: 'a) =
            Encode.buildWith builder a
            |> Json.formatWith options

module Optics =
    type Lens<'a,'b> = ('a -> JsonResult<'b>) * ('b -> 'a -> 'a)

    let get (l:Lens<'a,'b>) =
        fst l

    let set (l:Lens<'a,'b>) =
        snd l

    let compose (l1:Lens<'a,'b>) (l2:Lens<'b,'c>): Lens<'a,'c> =
        (fun a -> fst l1 a |> JsonResult.bind (fst l2)), (fun c a -> match JsonResult.map (snd l2 c) (fst l1 a) with JPass b -> snd l1 b a; | _ -> a)

    module JsonObject =
        let key_ k =
            JsonObject.find k, fun v jObj -> JsonObject.add k v jObj

    module Json =
        module E = Serialization.Json.Encode
        module D = Serialization.Json.Decode

        let Object__ = D.jsonObject, E.jsonObject
        let Array__ = D.list, E.array
        let String__ = D.string, E.string
        let Number__ = D.number, E.number
        let Bool__ = D.bool, E.bool
        let Null__ = D.unit, E.unit

        let Object_ = D.jsonObject, fun x _ -> E.jsonObject x
        let Array_ = D.list, fun x _ -> E.list x
        let String_ = D.string, fun x _ -> E.string x
        let Number_ = D.number, fun x _ -> E.number x
        let Bool_ = D.bool, fun x _ -> E.bool x
        let Null_ = D.unit, fun () _ -> E.unit ()
        let Int16_ : Lens<Json,int16> =
            D.int16, fun x _ -> E.int16 x

        let Int32_ : Lens<Json,int> =
            D.int, fun x _ -> E.int x

        let Int64_ : Lens<Json,int64> =
            D.int64, fun x _ -> E.int64 x

        let UInt16_ : Lens<Json,uint16> =
            D.uint16, fun x _ -> E.uint16 x

        let UInt32_ : Lens<Json,uint32> =
            D.uint32, fun x _ -> E.uint32 x

        let UInt64_ : Lens<Json,uint64> =
            D.uint64, fun x _ -> E.uint64 x

        let Single_ : Lens<Json,single> =
            D.single, fun x _ -> E.single x

        let Double_ : Lens<Json,float> =
            D.float, fun x _ -> E.float x

        let Decimal_ : Lens<Json,decimal> =
            D.decimal, fun x _ -> E.decimal x

        let BigInteger_ : Lens<Json,bigint> =
            D.bigint, fun x _ -> E.bigint x

        let DateTime_ : Lens<Json,System.DateTime> =
            D.dateTime, fun x _ -> E.dateTime x

        let DateTimeOffset_ : Lens<Json,System.DateTimeOffset> =
            D.dateTimeOffset, fun x _ -> E.dateTimeOffset x

        let Guid_ : Lens<Json,System.Guid> =
            D.guid, fun x _ -> E.guid x

        let Bytes_ : Lens<Json,byte array> =
            D.bytes, fun x _ -> E.bytes x

        let Property_ k =
            compose Object_ (JsonObject.key_ k)

module JsonTransformer =
    type Json<'a> = Json -> JsonResult<'a> * Json

    module Json =
        let init (a: 'a) : Json<'a> =
            fun json ->
                (JsonResult.pass a, json)

        let error (e: JsonFailure) : Json<'a> =
            fun json ->
                (JsonResult.fail e, json)

        let ofResult result : Json<'a> =
            fun json ->
                (result, json)

        let bind (a2bJ: 'a -> Json<'b>) (aJ: Json<'a>) : Json<'b> =
            fun json ->
                match aJ json with
                | JPass a, json' -> a2bJ a json'
                | JFail e, json' -> JsonResult.fail e, json'

        let apply (aJ: Json<'a>) (a2Jb: Json<'a -> 'b>) : Json<'b> =
            fun json ->
                match a2Jb json with
                | JPass a2b, json' ->
                    match aJ json' with
                    | JPass a, json'' -> JsonResult.pass (a2b a), json''
                    | JFail e, json'' -> JsonResult.fail e, json''
                | JFail e, json' -> JsonResult.fail e, json'

        let map (f: 'a -> 'b) (m: Json<'a>) : Json<'b> =
            fun json ->
                match m json with
                | JPass a, json -> JsonResult.pass (f a), json
                | JFail e, json -> JsonResult.fail e, json

        let map2 (a2b2c: 'a -> 'b -> 'c) (aJ: Json<'a>) (bJ: Json<'b>) : Json<'c> =
            map a2b2c aJ
            |> apply bJ

        let map3 (a2b2c2d: 'a -> 'b -> 'c -> 'd) (aJ: Json<'a>) (bJ: Json<'b>) (cJ: Json<'c>) : Json<'d> =
            map a2b2c2d aJ
            |> apply bJ
            |> apply cJ

        let map4 (a2b2c2d2x: 'a -> 'b -> 'c -> 'd -> 'x) (aJ: Json<'a>) (bJ: Json<'b>) (cJ: Json<'c>) (dJ: Json<'d>) : Json<'x> =
            map a2b2c2d2x aJ
            |> apply bJ
            |> apply cJ
            |> apply dJ

        let map5 (a2b2c2d2x2y: 'a -> 'b -> 'c -> 'd -> 'x -> 'y) (aJ: Json<'a>) (bJ: Json<'b>) (cJ: Json<'c>) (dJ: Json<'d>) (xJ: Json<'x>) : Json<'y> =
            map a2b2c2d2x2y aJ
            |> apply bJ
            |> apply cJ
            |> apply dJ
            |> apply xJ

        let map6 (a2b2c2d2x2y2z: 'a -> 'b -> 'c -> 'd -> 'x -> 'y -> 'z) (aJ: Json<'a>) (bJ: Json<'b>) (cJ: Json<'c>) (dJ: Json<'d>) (xJ: Json<'x>) (yJ: Json<'y>) : Json<'z> =
            map a2b2c2d2x2y2z aJ
            |> apply bJ
            |> apply cJ
            |> apply dJ
            |> apply xJ
            |> apply yJ

        let toJsonReader (aJ:Json<'a>): Decoder<Json,'a> =
            fun json ->
                aJ json |> fst

    module Operators =
        let inline (>>=) m f = Json.bind f m
        let inline (=<<) f m = Json.bind f m
        let inline (<*>) f m = Json.apply m f
        let inline (<!>) f m = Json.map f m
        let inline ( *>) m1 m2 = Json.map2 (fun _ x -> x) m1 m2
        let inline ( <*) m1 m2 = Json.map2 (fun x _ -> x) m1 m2
        let (>=>) m1 m2 = m1 >> Json.bind m2
        let (<=<) m2 m1 = m1 >> Json.bind m2

#nowarn "60"
type Json with
    override x.ToString() =
        Formatting.Json.format x

module Inference =
    module Internal =
        module E = Serialization.Json.Encode
        module D = Serialization.Json.Decode

        type ChironDefaults = ChironDefaults with
            static member ToJson (jObj: JsonObject): Json = E.jsonObject jObj
            static member ToJson (str: string): Json = E.string str
            static member ToJson (n: int16): Json = E.int16 n
            static member ToJson (n: int): Json = E.int n
            static member ToJson (n: int64): Json = E.int64 n
            static member ToJson (n: uint16): Json = E.uint16 n
            static member ToJson (n: uint32): Json = E.uint32 n
            static member ToJson (n: uint64): Json = E.uint64 n
            static member ToJson (n: single): Json = E.single n
            static member ToJson (n: float): Json = E.float n
            static member ToJson (n: decimal): Json = E.decimal n
            static member ToJson (n: bigint): Json = E.bigint n
            static member ToJson (b: bool): Json = E.bool b
            static member ToJson (dt: System.DateTime): Json = E.dateTime dt
            static member ToJson (dt: System.DateTimeOffset): Json = E.dateTimeOffset dt
            static member ToJson (g: System.Guid): Json = E.guid g
            static member ToJson (j: Json): Json = E.json j

            static member FromJson (_: JsonObject) = D.jsonObject
            static member FromJson (_: string) = D.string
            static member FromJson (_: int16) = D.int16
            static member FromJson (_: int) = D.int
            static member FromJson (_: int64) = D.int64
            static member FromJson (_: uint16) = D.uint16
            static member FromJson (_: uint32) = D.uint32
            static member FromJson (_: uint64) = D.uint64
            static member FromJson (_: single) = D.single
            static member FromJson (_: float) = D.float
            static member FromJson (_: decimal) = D.decimal
            static member FromJson (_: bigint) = D.bigint
            static member FromJson (_: bool) = D.bool
            static member FromJson (_: System.DateTime) = D.dateTime
            static member FromJson (_: System.DateTimeOffset) = D.dateTimeOffset
            static member FromJson (_: System.Guid) = D.guid
            static member FromJson (_: Json) = D.json

        let inline encodeWithDefaults (defaults: ^def) (a: ^a): Json =
            ((^a or ^def) : (static member ToJson : ^a -> Json) a)

        let inline encode (a: 'a) =
            encodeWithDefaults ChironDefaults a

        let inline decodeWithDefaults (defaults: ^def) (dummy: ^a): Decoder<Json,'a> =
            ((^a or ^def) : (static member FromJson : ^a -> Decoder<Json,'a>) dummy)

        let inline decode (json: Json) : JsonResult<'a> =
            decodeWithDefaults ChironDefaults Unchecked.defaultof<'a> json

        type ChironDefaults with
            static member inline ToJson (xs: 'a list): Json = E.listWith encode xs
            static member inline ToJson (xs: 'a array): Json = E.arrayWith encode xs
            static member inline ToJson (xO: 'a option): Json = E.optionWith encode xO
            static member inline ToJson (xs: Set<'a>): Json = E.setWith encode xs
            static member inline ToJson (m: Map<string, 'a>): Json = E.mapWith encode m
            static member inline ToJson (r: Result<'a, 'b>): Json = E.resultWith encode encode r
            static member inline ToJson (t): Json = E.tuple2 encode encode t
            static member inline ToJson (t): Json = E.tuple3 encode encode encode t
            static member inline ToJson (t): Json = E.tuple4 encode encode encode encode t
            static member inline ToJson (t): Json = E.tuple5 encode encode encode encode encode t
            static member inline ToJson (t): Json = E.tuple6 encode encode encode encode encode encode t
            static member inline ToJson (t): Json = E.tuple7 encode encode encode encode encode encode encode t

            static member inline FromJson (_: 'a list) = D.listWith decode
            static member inline FromJson (_: 'a array): Decoder<Json,'a array> = D.arrayWith decode
            static member inline FromJson (_: 'a option) = D.optionWith decode
            static member inline FromJson (_: Set<'a>) = D.setWith decode
            static member inline FromJson (_: Map<string, 'a>) = D.mapWith decode
            static member inline FromJson (_: Result<'a, 'b>) = D.resultWith decode decode
            static member inline FromJson (_: 'a * 'b) = D.tuple2With decode decode
            static member inline FromJson (_: 'a * 'b * 'c) = D.tuple3With decode decode decode
            static member inline FromJson (_: 'a * 'b * 'c * 'd) = D.tuple4With decode decode decode decode
            static member inline FromJson (_: 'a * 'b * 'c * 'd * 'e) = D.tuple5With decode decode decode decode decode
            static member inline FromJson (_: 'a * 'b * 'c * 'd * 'e * 'f) = D.tuple6With decode decode decode decode decode decode
            static member inline FromJson (_: 'a * 'b * 'c * 'd * 'e * 'f * 'g) = D.tuple7With decode decode decode decode decode decode decode

    module Json =
        let inline decodeWithDefaults (defaults: ^def) (a: ^a): Decoder<Json,'a> =
            Internal.decodeWithDefaults defaults a

        let inline decode (json: Json) : JsonResult<'a>=
            Internal.decode json

        let inline encodeWithDefaults (defaults: ^def) (a: ^a): Json =
            Internal.encodeWithDefaults defaults a

        let inline encode (a: 'a) =
            Internal.encode a

        let inline deserialize (str: string) =
            Json.parse str
            |> JsonResult.bind decode

        let inline serialize (a: 'a) =
            encode a
            |> Json.format

        module Encode =
            let inline requiredWithDefaults (defaults : ^def) (k : string) (v : ^a) (jObj : JsonObject) =
                JsonObject.add k (Internal.encodeWithDefaults defaults v) jObj

            let inline required k v (jObj : JsonObject) =
                JsonObject.add k (Internal.encode v) jObj

            let inline optionalWithDefaults (defaults : ^def) (k : string) (vO : ^a option) (jObj : JsonObject) =
                match vO with
                | Some v -> requiredWithDefaults defaults k v jObj
                | None -> jObj

            let inline optional k vO (jObj : JsonObject) =
                match vO with
                | Some v -> required k v jObj
                | None -> jObj

            let inline requiredMixin v jObj =
                (^a : (static member Mixin: ^a * JsonObject -> JsonObject) (v, jObj))

            let inline optionalMixin vO jObj =
                match vO with
                | Some v -> requiredMixin v jObj
                | None -> jObj

        module Decode =
            let inline requiredWithDefaults (defaults : ^def) (k : string) : Decoder<JsonObject,'a> =
                Json.Decode.required Internal.decode k

            let inline required (k: string) : Decoder<JsonObject,'a> =
                requiredWithDefaults Internal.ChironDefaults k

            let inline optionalWithDefaults (defaults : ^def) (k : string) : Decoder<JsonObject,'a option> =
                Json.Decode.optional Internal.decode k

            let inline optional (k: string) : Decoder<JsonObject,'a option> =
                optionalWithDefaults Internal.ChironDefaults k

            let inline requiredMixin (json: Json): JsonResult<'a> =
                (^a : (static member FromJson : ^a -> Decoder<Json,'a>) Unchecked.defaultof<'a>) json

module Builders =
    open JsonTransformer

    type JsonBuilder () =
        member __.Return (a : 'a) : Json<'a> = Json.init a
        member __.ReturnFrom (aJ) : Json<'a> = aJ
        member __.Bind (aJ, a2bJ) : Json<'a> = aJ |> Json.bind a2bJ
        member __.Zero () : Json<unit> = Json.init ()
        member __.Combine (m1, m2) : Json<'a> = m1 |> Json.bind (fun _ -> m2)
        member __.Delay (u2bJ) : Json<'a> = Json.init () |> Json.bind u2bJ

    type DecoderBuilder() =
        member __.Return (a: 'a) : Decoder<JsonObject,'a> = Decoder.alwaysPass a
        member __.ReturnFrom (aR: Decoder<JsonObject,'a>) = aR
        member __.Bind (aM, a2bM) : Decoder<JsonObject,'a> = aM |> Decoder.bind a2bM
        member __.Zero () : Decoder<JsonObject,unit> = Decoder.alwaysPass ()
        member __.Combine(r1, r2) : Decoder<JsonObject,'a> = r1 |> Decoder.bind (fun _ -> r2)
        member __.Delay(u2bM) : Decoder<JsonObject,'a> = Decoder.alwaysPass () |> Decoder.bind u2bM

        member __.Run (aM : Decoder<JsonObject,'a>) : Decoder<Json,'a> =
            Json.Decode.jsonObject |> Decoder.compose aM

    let json = JsonBuilder ()
    let jsonDecoder = DecoderBuilder()

[<AutoOpen>]
module Patterns =
    let (|Object|Array|String|Number|Bool|Null|) = function
        | Json.Object o -> Object o
        | Json.Array a -> Array a
        | Json.String s -> String s
        | Json.Number n -> Number n
        | Json.True -> Bool true
        | Json.False -> Bool false
        | Json.Null -> Null

    let (|PropertyWith|_|) (r: Decoder<Json,'a>) (key: string) (json: Json) : 'a option =
        Optics.get (Optics.Json.Property_ key) json
        |> JsonResult.bind r
        |> function
            | JPass x -> Some x
            | _ -> None

    let inline (|Property|_|) (key: string) (json: Json): 'a option =
        Optics.get (Optics.Json.Property_ key) json
        |> JsonResult.bind Inference.Json.decode
        |> function
            | JPass x -> Some x
            | _ -> None
