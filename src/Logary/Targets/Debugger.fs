/// The Debugger target is useful when running in Xamarin Studio or VS.
module Logary.Targets.Debugger

open System.IO
open System.Threading

// Ignore deprecations (Debug doesn't have a Stream-ish to write to)
#nowarn "44"

open System.Diagnostics
open Hopac
open Hopac.Infixes
open Logary
open Logary.Internals
open Logary.Configuration.Target

let defaultMessageFormat = SimpleMessageWriter() :> MessageWriter

type DebuggerConf =
  { writer: MessageWriter }

  /// Create a new Debugger configuration with a given writer (which
  /// formats how the Messages and Gauges/Derived-s are printed)
  [<CompiledName "Create">]
  static member create (?writer) =
    { writer = defaultArg writer defaultMessageFormat }

/// Default debugger configuration
let empty = DebuggerConf.create ()

module private Impl =

  let loop conf (api: TargetAPI) =
    let offLevel = 6

    let rec loop (): Job<unit> =
      Alt.choose [
        api.shutdownCh ^=> fun ack -> IVar.fill ack ()

        RingBuffer.take api.requests ^=> function
          | Log (message, ack) when Debugger.IsLogging() ->
            let path = PointName.format message.name
            job {
              use cts = new CancellationTokenSource()
              use sw = new StringWriter()
              do Debugger.Log(offLevel, path, sw.ToString())
              do! Job.fromUnitTask (fun () -> conf.writer.write(sw, message, cts.Token))
              do! ack *<= ()
              return! loop ()
            }

          | Log (_, ack)
          | Flush (ack, _) ->
            IVar.fill ack () >>= loop
      ] :> Job<_>

    loop ()

[<CompiledName "Create">]
let create conf name =
  TargetConf.createSimple (Impl.loop conf) name

/// Use with LogaryFactory.New( s => s.Target<Debugger.Builder>() )
type Builder(conf, callParent: ParentCallback<Builder>) =

  /// Specify the formatting style to use when logging to the debugger
  member x.WithFormatter( sf: MessageWriter ) =
    ! (callParent <| Builder({ conf with writer = sf }, callParent))

  new(callParent: ParentCallback<_>) =
    Builder(empty, callParent)

  interface SpecificTargetConf with
    member x.Build name = create conf name

