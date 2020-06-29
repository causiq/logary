namespace Logary.Internals

open System
open System.Threading.Tasks
open FSharp.Control.Tasks.Builders
open Logary
open Logary.Json
open Logary.Internals
open Logary.Internals.Chiron

type JSONMessageWriter(?formattingOptions) =
  let fo = defaultArg formattingOptions JsonFormattingOptions.SingleLine
  interface MessageWriter with
    member x.write(writer, message, ct) =
      task {
        let str =
          Encode.logaryMessageBase message
            |> Json.formatWith fo // TODO: allow JSON formatter to use existing memory
        do! writer.WriteLineAsync(str.AsMemory(), ct)
      }
      :> Task
