/// The System.Diagnostics.Trace Target for Logary
module Logary.Targets.DiagnosticsTrace

open System.Diagnostics
open System.IO
open Logary
open Logary.Configuration.Target
open Logary.Internals

/// Console configuration structure.
type TraceConf =
  { writer: MessageWriter }

  [<CompiledName "Create">]
  static member create writer =
    { writer = writer }


let defaultMessageFormat = SimpleMessageWriter() :> MessageWriter


/// Default console target configuration.
let empty =
  TraceConf.create defaultMessageFormat

module internal Impl =
  open Hopac
  open Hopac.Infixes

  let loop (conf: TraceConf) (api: TargetAPI) =

    let rec loop (): Job<unit> =
      Alt.choose [
        api.shutdownCh ^=> fun ack ->
          ack *<= () :> Job<_>

        RingBuffer.take api.requests ^=> function
          | Log (message, ack) ->
            job {
              use sw = new StringWriter()
              do! Alt.fromUnitTask (fun ct -> conf.writer.write(sw, message, ct))
              do Trace.WriteLine(sw.ToString())
              do! ack *<= ()
              return! loop ()
            }

          | Flush (ack, _) ->
            IVar.fill ack () >>= loop

      ] :> Job<_>

    loop ()

[<CompiledName "Create">]
let create conf name =
  TargetConf.createSimple (Impl.loop conf) name

/// Use with LogaryFactory.New( s => s.Target<Console.Builder>() )
type Builder(conf, callParent: ParentCallback<Builder>) =

  /// Specify the formatting style to use when logging to the console
  member x.WithFormatter( sf: MessageWriter ) =
    ! (callParent <| Builder({ conf with writer = sf }, callParent))

  new(callParent: ParentCallback<_>) =
    Builder(empty, callParent)

  interface SpecificTargetConf with
    member x.Build name = create conf name
