namespace Logary.Internals

open System.Collections.Generic
open System.Text.RegularExpressions
open FSharp.Control.Tasks.Builders
open System
open Logary
open Logary.Internals
open NodaTime

module SimpleFormatting =

  let private varRegex = Regex("\\{([a-zA-Z._-]+)\\}", RegexOptions.Compiled ||| RegexOptions.CultureInvariant ||| RegexOptions.IgnoreCase)

  let private tryGet (source: IReadOnlyDictionary<_,_>) key =
    match source.TryGetValue key with
    | false, _ -> None
    | true, v -> Some (v.ToString())

  let template (template: string) (fields: IReadOnlyDictionary<string, Value>, context: IReadOnlyDictionary<string, Value>): string * Set<string> =
    let usedVars = ref Set.empty
    let evaluator (m: Match): string = // evaluate each group individually and construct a string
      if m.Groups.Count <= 1 then m.Value else
      let key = m.Groups.[1].Value
      match tryGet fields key |> Option.orElseWith (fun () -> tryGet context key) with
      | Some found ->
        usedVars := !usedVars |> Set.add key
        found
      | None ->
        m.Value
    varRegex.Replace(template, evaluator),
    !usedVars

  let private kvsRegex = Regex("[\\\"]", RegexOptions.Compiled ||| RegexOptions.CultureInvariant ||| RegexOptions.IgnoreCase)
  let private spaceRegex = Regex("\\s", RegexOptions.Compiled)

  let private escapeValue (value: string): string =
    let inner (m: Match): string =
      match m.Value with
      | "\\" -> "\\\\"
      | "\"" -> "\\\""
      | _ -> m.Value
    let quoteIfHasSpace (s: string) =
      if spaceRegex.IsMatch s then String.Concat([| "\""; s; "\"" |]) else s
    kvsRegex.Replace(value, inner) |> quoteIfHasSpace

  let kv (k, v) = sprintf "%s=%s" (escapeValue k) (escapeValue v)
  let kvs (kvs: seq<string * string>) = kvs |> Seq.map kv

  /// Creates key-value pairs for the SimpleMessageWriter to print
  let tokenise (message: LogaryMessage): seq<string * string> =
    seq {
      let used = HashSet<string>()
      yield "lvl", message.level.ToString()
      let ts = Instant.FromUnixTimeTicks (message.timestamp / Constants.NanosPerTick)
      yield "ts", (ts.ToString("yyyy'-'MM'-'dd'T'HH':'mm':'ss'.'fff'Z'", Culture.invariant))

      match message with
      | :? EventMessage as e ->
        let templated, usedVars = template e.event (message.fields, message.context)
        usedVars |> Seq.iter (used.Add >> ignore)
        yield "evt", templated
        if e.monetaryValue.IsSome then
          yield "money", e.monetaryValue.Value.ToString()

      | :? SpanMessage as s ->
        yield "lbl", s.label
        yield "dur", s.elapsed.toGauge().ToString()

      | :? GaugeMessage as g ->
        yield "g", g.ToString()

      | :? HistogramMessage as h ->
        ignore h

      | _ ->
        ignore message
        ()

      if message.received.IsSome then
        let ts = Instant.ofEpoch message.received.Value
        yield "recv", (ts.ToString("yyyy'-'MM'-'dd'T'HH':'mm':'ss'.'fff'Z'", Culture.invariant))

      if message.parentSpanId.IsSome then
        yield "parent", message.parentSpanId.Value.toBase64String()

      yield!
        message.fields
          |> Seq.map (fun (KeyValue (k, v)) -> k, v.ToString())
          |> Seq.append (message.context |> Seq.map (fun (KeyValue (k, v)) -> k, v.ToString()))
          |> Seq.filter (fun (k, _) -> not (used.Contains k))
          |> Seq.filter (fun (k, v) -> not (String.IsNullOrWhiteSpace k) && not (String.IsNullOrWhiteSpace v))

      yield "name", message.name.ToString()
      yield "type", message.kind.ToString()

      match message.tryGetAs<EventMessage>() with
      | Some event ->
        if event.error.IsSome then
          let error = event.error.Value :> IValueFormattable
          match error.toKeyValues "error" with
          | Choice1Of2 kv ->
            yield kv.Key, kv.Value.ToString()
          | Choice2Of2 kvs ->
            for kv in kvs do
              yield kv.Key, kv.Value.ToString()
      | None ->
        ()
    }


open SimpleFormatting

type SimpleMessageWriter() =
  interface MessageWriter with
    member x.write(writer, message, ct) =
      task {
        let tokens = tokenise message
        let mutable first = true
        for token in tokens do
          let escaped = kv token
          if first then
            do! writer.WriteAsync(escaped.PadRight(10).AsMemory(), ct)
            first <- false
          else
            do! writer.WriteAsync(" ".AsMemory(), ct)
            do! writer.WriteAsync(escaped.AsMemory(), ct)

        do! writer.WriteLineAsync()
      }
      :> _
