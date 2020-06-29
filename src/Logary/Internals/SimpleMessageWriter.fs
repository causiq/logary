namespace Logary.Internals

open FSharp.Control.Tasks.Builders
open System
open Logary
open Logary.Internals

type SimpleMessageWriter() =
  interface MessageWriter with
    member x.write(writer, message, ct) =
      task {
        if ct.IsCancellationRequested then return ()

        do! writer.WriteAsync(message.level.ToString().PadRight(8, ' ').AsMemory(), ct)
        if ct.IsCancellationRequested then return ()

        do! writer.WriteAsync(' ')
        if ct.IsCancellationRequested then return ()

        match message with
        | :? EventMessage as e ->
          do! writer.WriteAsync(e.event.AsMemory(), ct)
          if ct.IsCancellationRequested then return ()

        | :? SpanMessage as s ->
          do! writer.WriteAsync(s.label.AsMemory(), ct)
          if ct.IsCancellationRequested then return ()

          do! writer.WriteAsync(s.elapsed.toGauge().ToString().AsMemory(), ct)
          if ct.IsCancellationRequested then return ()

        | :? GaugeMessage as g ->
          do! writer.WriteAsync(g.ToString().AsMemory(), ct)
          if ct.IsCancellationRequested then return ()

        | :? HistogramMessage as h ->
          ignore h

        | _ -> ()

        do! writer.WriteLineAsync()
      }
      :> _
