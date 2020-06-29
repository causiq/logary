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
    /// The error text writer to output to
    error: TextWriter
    /// Whether to flush text writer after each line
    flush: bool
    /// Whether to add Logary's RuntimeInfo's Resource value to the context.
    includeResource: bool
    /// The log level that is considered 'important' enough to write to the
    /// error text writer
    useErrorFor: LogLevel }

  [<CompiledName "Create">]
  static member create(output, error, ?formatter: MessageWriter) =
    { writer = defaultArg formatter defaultMessageFormat
      output = output
      error  = error
      flush  = false
      includeResource = true
      useErrorFor     = Error }

module internal Impl =

  let loop (conf: TextWriterConf) (api: TargetAPI) =
    let withConsoleLock x = DVar.mapFun Lock.duringJob api.runtime.consoleLock x

    let rec loop (): Job<unit> =
      Alt.choose [
        api.shutdownCh ^=> fun ack ->
          conf.output.Dispose()

          if not (obj.ReferenceEquals(conf.output, conf.error)) then
            conf.error.Dispose()

          ack *<= ()

        RingBuffer.take api.requests ^=> function
          | Log (message, ack) ->
            job {
              let writer = if message.level < conf.useErrorFor then conf.output else conf.error

              let message =
                if conf.includeResource then
                  message.cloneAndUpdate(fun m -> m.setContextValues(api.runtime.resource.asMap()))
                else
                  message

              let xJ = Alt.fromUnitTask (fun ct -> conf.writer.write(writer, message, ct)) :> Job<_>
              do! withConsoleLock xJ

              if conf.flush then
                do! Job.fromUnitTask (fun _ -> writer.FlushAsync())

              do! ack *<= ()
              return! loop ()
            }

          | Flush (ack, _) ->
            Job.fromUnitTask conf.output.FlushAsync
            >>=. Job.fromUnitTask conf.error.FlushAsync
            >>= IVar.fill ack
            >>= loop

      ] :> Job<_>

    loop ()

[<CompiledName "Create">]
let create (conf: TextWriterConf) name =
  TargetConf.createSimple (Impl.loop conf) name

/// Use with LogaryFactory.New( s => s.Target<TextWriter.Builder>() )
type Builder(conf, callParent: ParentCallback<Builder>) =
  member x.WriteTo(out : #TextWriter, err : #TextWriter) =
    ! (callParent <| Builder({ conf with output = out; error = err }, callParent))

  new(callParent: ParentCallback<_>) =
    Builder(TextWriterConf.create(System.Console.Out, System.Console.Error), callParent)

  interface SpecificTargetConf with
    member x.Build name = create conf name

