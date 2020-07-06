/// A module implementing a text writer target. Useful for writing to the console output, or writing to a custom text writer.
module Logary.Targets.TextWriter

open System.IO
open Hopac
open Hopac.Infixes
open Logary
open Logary.Internals
open Logary.Configuration.Target

let defaultMessageFormat = JSONMessageWriter() :> MessageWriter

/// Configuration for a text writer
type TextWriterConf =
  { /// A message writer to specify how to write the Message.
    writer: MessageWriter
    /// The non-error text writer to output to
    output: TextWriter
    /// Whether to flush text writer after each line
    flush: bool
    /// Whether to add Logary's RuntimeInfo's Resource value to the context.
    includeResource: bool }

  [<CompiledName "Create">]
  static member create(output, ?formatter: MessageWriter, ?flush, ?includeResource) =
    { writer = defaultArg formatter defaultMessageFormat
      output = output
      flush  = defaultArg flush false
      includeResource = defaultArg includeResource false }

module internal Impl =

  let loop (conf: TextWriterConf) (api: TargetAPI) =
    let withConsoleLock x = DVar.mapFun Lock.duringJob api.runtime.consoleLock x

    let rec loop (): Job<unit> =
      Alt.choose [
        api.shutdownCh ^=> fun ack ->
          conf.output.Dispose()
          ack *<= ()

        RingBuffer.take api.requests ^=> function
          | Log (message, ack) ->
            job {
              let message =
                if conf.includeResource then
                  message.cloneAndUpdate(fun m -> m.setContextValues(api.runtime.resource.asMap()))
                else
                  message

              let xJ = Alt.fromUnitTask (fun ct -> conf.writer.write(conf.output, message, ct)) :> Job<_>
              do! withConsoleLock xJ

              if conf.flush then
                do! Job.fromUnitTask (fun _ -> conf.output.FlushAsync())

              do! ack *<= ()
              return! loop ()
            }

          | Flush (ack, _) ->
            Job.fromUnitTask conf.output.FlushAsync
            >>= IVar.fill ack
            >>= loop

      ] :> Job<_>

    loop ()

[<CompiledName "Create">]
let create (conf: TextWriterConf) name =
  TargetConf.createSimple (Impl.loop conf) name

/// Use with LogaryFactory.New( s => s.Target<TextWriter.Builder>() )
type Builder(conf, callParent: ParentCallback<Builder>) =
  member x.WriteTo(out : #TextWriter) =
    ! (callParent <| Builder({ conf with output = out }, callParent))

  member x.IncludeResource() =
    callParent (Builder({ conf with includeResource = true }, callParent))

  new(callParent: ParentCallback<_>) =
    Builder(TextWriterConf.create(System.Console.Out), callParent)

  interface SpecificTargetConf with
    member x.Build name = create conf name

