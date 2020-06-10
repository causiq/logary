/// The console Target for Logary
module Logary.Targets.Console

open Logary
open Logary.Configuration.Target
open Logary.Internals

/// Console configuration structure.
type ConsoleConf =
  { writer: MessageWriter }

  [<CompiledName "Create">]
  static member create writer =
    { writer = writer }

let defaultMessageFormat = SimpleMessageWriter() :> MessageWriter

/// Default console target configuration.
let empty =
  ConsoleConf.create defaultMessageFormat

[<CompiledName "Create">]
let create conf name =
  TextWriter.create
    { writer    = conf.writer
      output    = System.Console.Out
      error     = System.Console.Error
      flush     = false
      useErrorFor = Error }
    name

/// Use with LogaryFactory.New( s => s.Target<Console.Builder>() )
type Builder(conf, callParent: ParentCallback<Builder>) =

  /// Specify the formatting style to use when logging to the console
  member x.WithFormatter( sf: MessageWriter ) =
    ! (callParent <| Builder({ conf with writer = sf }, callParent))

  new(callParent: ParentCallback<_>) =
    Builder(empty, callParent)

  interface SpecificTargetConf with
    member x.Build name = create conf name
