/// The console Target for Logary
module Logary.Targets.Console

open System.IO
open Logary.Configuration.Target
open Logary.Internals

/// Console configuration structure.
type ConsoleConf =
  { writer: MessageWriter
    output: TextWriter
    flush: bool
    includeResource: bool }
  member x.toTextWriterConf() =
    { TextWriter.writer = x.writer
      TextWriter.output = x.output
      TextWriter.flush = x.flush
      TextWriter.includeResource = x.includeResource }
  [<CompiledName "Create">]
  static member create(writer, ?output, ?flush, ?includeResource) =
    { writer = writer
      output = defaultArg output System.Console.Out
      flush = defaultArg flush false
      includeResource = defaultArg includeResource false }

let defaultMessageWriter = SimpleMessageWriter() :> MessageWriter

/// Default console target configuration.
let empty =
  ConsoleConf.create defaultMessageWriter

[<CompiledName "Create">]
let create (conf: ConsoleConf) name =
  TextWriter.create (conf.toTextWriterConf()) name

/// Use with LogaryFactory.New( s => s.Target<Console.Builder>() )
type Builder(conf, callParent: ParentCallback<Builder>) =

  /// Specify the formatting style to use when logging to the console
  member x.WithFormatter( sf: MessageWriter ) =
    ! (callParent <| Builder({ conf with writer = sf }, callParent))

  new(callParent: ParentCallback<_>) =
    Builder(empty, callParent)

  interface SpecificTargetConf with
    member x.Build name = create conf name
