// ----------------------------------------------------------------------------------------------
// Copyright 2016 Mårten Rånge
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
// ----------------------------------------------------------------------------------------------

namespace Logary

module HashMap =
  open Logary.Internals
  open System.Collections.Generic

  let inline containsKey (key: 'K) (m: HashMap<'K, 'V>): bool =
    match m.TryFind key with
    | true, _ -> true
    | _   , _ -> false

  [<GeneralizableValue>]
  let empty<'K, 'V when 'K :> System.IEquatable<'K>> : HashMap<_, _> = upcast HashMap<'K, 'V>.Empty

  let inline isEmpty (m: HashMap<'K, 'V>): bool =
    m.IsEmpty

  let inline tryFind (key: 'K) (m: HashMap<'K, 'V>) : 'V option =
    match m.TryFind key with
    | true, v -> Some v
    | _   , _ -> None

  let inline set (key: 'K) (value: 'V) (m: HashMap<'K, 'V>): HashMap<'K, 'V> =
    if isNull (box key) then nullArg "key"
    m.Set key value

  /// Alias for `set`
  let inline add key value m = set key value m

  let inline unset (key: 'K) (m: HashMap<'K, 'V>): HashMap<'K, 'V> =
    m.Unset key

  /// Alias for `unset`
  let inline remove key m = unset key m

  let inline visit (visitor: 'K -> 'V -> bool) (m: HashMap<'K, 'V>): bool =
    m.Visit visitor

  let inline length (m: HashMap<'K, 'V>): int =
    let l = ref 0
    visit (fun k v -> incr l; true) m |> ignore
    !l

  let inline toArray (m: HashMap<'K, 'V>): KeyValuePair<'K, 'V> [] =
    let ra = ResizeArray<_> 16
    visit (fun k v -> ra.Add (KeyValuePair (k, v)); true) m |> ignore
    ra.ToArray ()

  let inline toSeqPair (m: HashMap<'K, 'V>): seq<KeyValuePair<'K, 'V>> =
    upcast m

  let inline toSeq (m: HashMap<'K, 'V>): seq<'K * 'V>=
    toSeqPair m |> Seq.map (fun (KeyValue (k, v)) -> k, v)

  let inline toListPair (m: HashMap<'K, 'V>): KeyValuePair<'K, 'V> list =
    toSeqPair m |> List.ofSeq

  let inline toList (m: HashMap<'K, 'V>): ('K * 'V) list =
    toListPair m |> List.map (fun (KeyValue (k, v)) -> k, v)

  let inline toDictionary (m: HashMap<'K, 'V>): Dictionary<'K, 'V> =
    let d = Dictionary<'K, 'V>(length m)
    toSeqPair m |> Seq.iter (fun kvp -> d.Add(kvp.Key, kvp.Value))
    d

  let inline ofArrayPair (xs: KeyValuePair<'k, 'v> []): HashMap<'k, 'v> =
    xs |> Array.fold (fun map (KeyValue (k, v)) -> map |> add k v) empty

  let inline ofArray (xs: ('k * 'v) []): HashMap<'k, 'v> =
    xs |> Array.fold (fun map (k, v) -> map |> add k v) empty

  let inline ofSeqPair (xs: KeyValuePair<'k, 'v> seq): HashMap<'k, 'v> =
    xs |> Seq.fold (fun map (KeyValue (k, v)) -> map |> add k v) empty

  let inline ofSeq (xs: ('k * 'v) seq): HashMap<'k, 'v> =
    xs |> Seq.fold (fun map (k, v) -> map |> add k v) empty

  let inline ofListPair (xs: KeyValuePair<'k, 'v> list): HashMap<'k, 'v> =
    xs |> List.fold (fun map (KeyValue (k, v)) -> map |> add k v) empty

  let inline ofList (xs: ('k * 'v) list): HashMap<'k, 'v> =
    xs |> List.fold (fun map (k, v) -> map |> add k v) empty

  module Optic =
    open Logary.Internals.Aether

    /// Prism to a value associated with a key in a map.
    let key_ (k: 'k): Prism<HashMap<'k,'v>,'v> =
      tryFind k,
      (fun v x ->
        if containsKey k x then x else set k v x )

    /// Lens to a value option associated with a key in a map.
    let value_ (k: 'k): Lens<HashMap<'k,'v>, 'v option> =
      tryFind k,
      (fun v x ->
        match v with
        | Some v -> set k v x
        | _ -> unset k x)

