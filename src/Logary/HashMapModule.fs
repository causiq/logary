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

  let inline containsKey (key : 'K) (m : HashMap<'K, 'V>) : bool =
    match m.TryFind key with
    | true, _ -> true
    | _   , _ -> false

  [<GeneralizableValue>]
  let empty<'K, 'V when 'K :> System.IEquatable<'K>> : HashMap<_, _> = upcast HashMap<'K, 'V>.Empty

  let inline isEmpty (m : HashMap<'K, 'V>) : bool =
    m.IsEmpty

  let inline tryFind (key : 'K) (m : HashMap<'K, 'V>) : 'V option =
    match m.TryFind key with
    | true, v -> Some v
    | _   , _ -> None

  let inline set (key : 'K) (value : 'V) (m : HashMap<'K, 'V>) : HashMap<'K, 'V> =
    m.Set key value

  let inline unset (key : 'K) (m : HashMap<'K, 'V>) : HashMap<'K, 'V> =
    m.Unset key

  let inline visit (visitor : 'K -> 'V -> bool) (m : HashMap<'K, 'V>) : bool =
    m.Visit visitor

  let inline toArray (m : HashMap<'K, 'V>) : ('K*'V) [] =
    let ra = ResizeArray<_> 16
    visit (fun k v -> ra.Add (k, v); true) m |> ignore
    ra.ToArray ()

  let inline length (m : HashMap<'K, 'V>) : int =
    let l = ref 0
    visit (fun k v -> incr l; true) m |> ignore
    !l

