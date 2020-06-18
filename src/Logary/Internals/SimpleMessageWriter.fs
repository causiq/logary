namespace Logary.Internals

open FSharp.Control.Tasks.Builders
open System
open Logary
open Logary.Internals

type SimpleMessageWriter() =
  interface MessageWriter with
    member x.write(writer, message, ct) =
      task {
        do! writer.WriteAsync(message.level.ToString().PadRight(8, ' ').AsMemory(), ct)
        do! writer.WriteAsync(' ')

        match message with
        | :? EventMessage as e ->
          do! writer.WriteAsync(e.event.AsMemory(), ct)

        | :? SpanMessage as s ->
          do! writer.WriteAsync(s.label.AsMemory(), ct)
          do! writer.WriteAsync(s.elapsed.toGauge().ToString().AsMemory(), ct)

        | :? GaugeMessage as g ->
          do! writer.WriteAsync(g.ToString().AsMemory(), ct)

        | :? HistogramMessage as g ->
          ignore g

        | _ -> ()

        do! writer.WriteLineAsync()
      }
      :> _