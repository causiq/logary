namespace Logary.Trace.Propagation

open System
open System
open System.Collections.Generic
open System.Net.Http

module Extract =

  /// Gets a single item from the `source` using the `getter`.
  let internal getSingleExact (getter: Getter<_>) source header =
    getter source header |> List.tryPick (function
      | name, value :: _ when name.ToLowerInvariant() = header -> Some value
      | _ -> None)

  let internal getManyExact (get: Getter<_>) source header =
    get source header |> List.choose (function
      | name, values when name.ToLowerInvariant() = header -> Some values
      | _ -> None)

  let mapWithSeq: Getter<Map<string, _>> =
    fun source nameOrPrefix ->
      source
        |> Seq.filter (fun (KeyValue (key, _)) -> key.StartsWith(nameOrPrefix, StringComparison.InvariantCultureIgnoreCase))
        |> Seq.map (fun (KeyValue (k, values)) -> k, List.ofSeq values)
        |> List.ofSeq

  let mapWithSingle: Getter<Map<string, string>> =
    fun source nameOrPrefix ->
      source
        |> Seq.filter (fun (KeyValue (key, _)) -> key.StartsWith(nameOrPrefix, StringComparison.InvariantCultureIgnoreCase))
        |> Seq.map (fun (KeyValue (k, value)) -> k, value :: [])
        |> List.ofSeq

  let mapWithList: Getter<Map<string, _>> =
    fun source nameOrPrefix ->
      source
        |> Seq.filter (fun (KeyValue (key, _)) -> key.StartsWith(nameOrPrefix, StringComparison.InvariantCultureIgnoreCase))
        |> Seq.map (fun (KeyValue (k, values)) -> k, values)
        |> List.ofSeq

  // Dictionary
  let dictionaryWithSeq: Getter<IDictionary<string, _>> =
    fun source nameOrPrefix ->
      source
        |> Seq.filter (fun (KeyValue (key, _)) -> key.StartsWith(nameOrPrefix, StringComparison.InvariantCultureIgnoreCase))
        |> Seq.map (fun (KeyValue (k, values)) -> k, List.ofSeq values)
        |> List.ofSeq

  let dictionaryWithSingle: Getter<IDictionary<string, string>> =
    fun source nameOrPrefix ->
      source
        |> Seq.filter (fun (KeyValue (key, _)) -> key.StartsWith(nameOrPrefix, StringComparison.InvariantCultureIgnoreCase))
        |> Seq.map (fun (KeyValue (k, value)) -> k, value :: [])
        |> List.ofSeq

  // List

  /// IRL you may get multiple headers with the same keys, each containing a list of values
  let listWithList: Getter<(string * string list) list> =
    fun source nameOrPrefix ->
      source
        |> List.filter (fun (k, _) -> k.StartsWith(nameOrPrefix, StringComparison.InvariantCultureIgnoreCase))

module Inject =

  // Maps
  let mapWithSeq: Setter<Map<string, seq<string>>> =
    fun (name, values) target ->
      target
        |> Map.add name (values :> seq<_>)

  let mapWithList: Setter<Map<string, string list>> =
    fun (name, values) target ->
      target
        |> Map.add name values

  let mapWithArray: Setter<Map<string, string[]>> =
    fun (name, values) target ->
      target
        |> Map.add name (Array.ofList values)


  // Dictionaries
  let dictionaryWithSeq: Setter<IDictionary<string, seq<string>>> =
    fun (name, values) target ->
      target.[name] <- values :> seq<string>
      target

  let dictionaryWithSeqCopy: Setter<IDictionary<string, seq<string>>> =
    fun (name, values) target ->
      let copy = new Dictionary<_,_>(target)
      copy.[name] <- values
      copy :> IDictionary<_,_>


  let dictionaryWithList: Setter<IDictionary<string, string list>> =
    fun (name, values) target ->
      target.[name] <- values
      target

  let dictionaryWithListCopy: Setter<IDictionary<string, string list>> =
    fun (name, values) target ->
      let copy = new Dictionary<_,_>(target)
      copy.[name] <- values
      copy :> IDictionary<_,_>


  let dictionaryWithArray: Setter<IDictionary<string, string[]>> =
    fun (name, values) target ->
      target.[name] <- Array.ofList values
      target

  let dictionaryWithArrayCopy: Setter<IDictionary<string, string[]>> =
    fun (name, values) target ->
      let copy = new Dictionary<_,_>(target)
      copy.[name] <- Array.ofList values
      copy :> IDictionary<_,_>


  // HttpClient

  let httpRequestMessage: Setter<HttpRequestMessage> =
    fun (name, value) target ->
      target.Headers.Add(name, value)
      target