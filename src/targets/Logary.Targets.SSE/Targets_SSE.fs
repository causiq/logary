module Logary.Targets.SSE

open System.Runtime.CompilerServices
open Hopac
open Hopac.Infixes
open Logary
open Logary.Message
open Logary.Internals
open Logary.Configuration.Target

[<assembly:InternalsVisibleTo("Logary.Targets.SSE.Tests")>]
do()

// NOTE: this target is in ALPHA state (and it's fairly simple)
type SSEConf =
  { path: string
    ip: string
    port: int
  }
  static member create (?path, ?ip, ?port) =
    { path = defaultArg path "/"
      ip = defaultArg ip "0.0.0.0"
      port = defaultArg port 8080
    }

let empty = SSEConf.create()

module internal Impl =

  open System.Threading
  open Logary.Adapters.Facade
  open Logary.Formatting
  open Suave

  module History =
    type Container<'a> =
      { values: 'a []
        pos: uint16
        len: uint16 }

    let create (len: uint16) =
      { values = Array.zeroCreate (int len)
        pos = 0us
        len = 0us }

    let add c x =
      let pos' = c.pos % uint16 c.values.Length
      Array.set c.values (int pos') x
      { c with pos = pos' + 1us
               len = if pos' < c.pos then c.len else c.len + 1us }

    let read (c: Container<'a>) =
      let clone = c.values.Clone() :?> 'a []
      let rec reader remaining pos = seq {
        if remaining <> 0us then
          let pos' = pos % uint16 c.values.Length
          yield c.values.[int pos']
          yield! reader (remaining - 1us) (pos' + 1us)
        }
      reader c.len (c.pos + (uint16 c.values.Length - c.len))
    
  let createCTS (cancelled: Promise<unit>) =
    let cts = new CancellationTokenSource()
    start (cancelled >>- fun () -> cts.Cancel())
    cts

  let api (conf: SSEConf) ilogger = Successful.OK "TODO"

  let listen conf ilogger (cancelled: Promise<unit>) =
    job {
      let started, shutdown = IVar (), IVar ()
      let logging =
        { Suave.Logging.Global.defaultConfig with
            getLogger = fun name ->
              ilogger
              |> Logger.setNameEnding (String.concat "-" name)
              |> LoggerAdapter.createGeneric<Suave.Logging.Logger>
        }
      Suave.Logging.Global.initialiseIfDefault logging
      let bindings = HttpBinding.createSimple HTTP conf.ip conf.port :: []
      do ilogger.info (eventX "Starting HTTP recv-loop at {bindings}." >> setField "bindings" bindings)
      use cts = createCTS cancelled
      let suaveConfig = { Web.defaultConfig with cancellationToken = cts.Token
                                                 bindings = bindings }
      let webStarted, webListening = startWebServerAsync suaveConfig (api conf ilogger)
      do! Job.start (Job.fromAsync webListening |> Job.bind (IVar.fill shutdown))
      do! Job.start (Job.fromAsync webStarted |> Job.bind (fun _ -> IVar.fill started ()))
      do! cancelled
      do ilogger.info (eventX "Stopping HTTP recv-loop at {bindings}." >> setField "bindings" bindings)
      return started, shutdown
    }
      
  type Message with
    member x.toJSONMessage() =
      Json.encode x
  
  type State =
    { shutdown: Promise<unit>
      cancelled: IVar<unit>
    }
  
  // This is the main entry point of the target. It returns a Job<unit>
  // and as such doesn't have side effects until the Job is started.
  let loop (conf: SSEConf) (api: TargetAPI) =
    let ilogger = api.runtime.logger

    let rec initialise () =
      job {
        let cancelled = IVar ()
        let! started, shutdown = listen conf ilogger cancelled
        let state = { cancelled=cancelled; shutdown=shutdown }
        return! running state
      }
      
    and running (state: State): Job<_> =
      Alt.choose [
        RingBuffer.takeBatch 10us api.requests ^=> fun messages ->
          let entries, acks, flushes =
            messages |> Array.fold (fun (entries, acks, flushes) -> function
              | Log (message, ack) ->
                message.toJSONMessage() :: entries,
                ack *<= () :: acks,
                flushes
              | Flush (ackCh, nack) ->
                entries,
                acks,
                ackCh *<= () :: flushes)
              ([], [], [])
              
          job {
            do ilogger.verbose (eventX "Writing {count} messages." >> setField "count" entries.Length)

            // TODO: send
            // do! state.send entries

            do ilogger.verbose (eventX "Acking messages.")
            do! Job.conIgnore acks
            do! Job.conIgnore flushes
            return! running state
          }

        api.shutdownCh ^=> fun ack ->
          state.cancelled *<= ()
          >>=. state.shutdown
          >>=. ack *<= ()
      ] :> Job<_>

    initialise ()

/// Create a new BigQuery target
[<CompiledName "Create">]
let create conf name =
  TargetConf.createSimple (Impl.loop conf) name

/// Use with LogaryFactory.New(s => s.Target<BigQuery>())
type Builder(conf, callParent: ParentCallback<Builder>) =
  let update (conf': SSEConf): Builder =
    Builder(conf', callParent)
  member x.Path(path: string) =
    update { conf with path = path }
  new(callParent: ParentCallback<_>) =
    Builder(empty, callParent)

  interface SpecificTargetConf with
    member x.Build name = create conf name
