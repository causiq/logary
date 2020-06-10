namespace Logary.Ingestion

open System.Text
open System.IO
open Logary
open Logary.Internals
open Hopac
open fszmq

type TCPConfig =
  { cancelled: Promise<unit>
    ilogger: Logger
    /// TCP-ingestion server specific information
    createSocket: Context -> Socket
  }

  interface IngestServerConfig with
    member x.cancelled = x.cancelled
    member x.ilogger = x.ilogger

  static member create createSocket cancelled ilogger =
    { cancelled = cancelled
      createSocket = createSocket
      ilogger = ilogger }

module TCP =
  let recv (started: IVar<unit>, shutdown: IVar<unit>) (config: TCPConfig) (next: Ingest) =
    Job.Scheduler.isolate <| fun () ->
    config.ilogger.info "Starting ZMQ STREAM/TCP recv-loop."
    // https://gist.github.com/lancecarlson/fb0cfd0354005098d579
    // https://gist.github.com/claws/7231548#file-czmq-stream-server-c-L21
    // In https://github.com/zeromq/libzmq/issues/1573 we discovered that;
    // the thread that called new Context() must be preserved
    use context = new Context()
    use socket = config.createSocket context
    try
      try
        start (IVar.fill started ())
        while not (Promise.Now.isFulfilled config.cancelled) do
          let frame = Socket.recv socket
          // Aborted socket due to closing process:
          if isNull frame then () else
          let bs = Socket.recv socket
          // http://api.zeromq.org/4-1:zmq-socket#toc19
          // A connection was made
          if bs.Length = 0 then () else
          use ms = new MemoryStream(bs)
          use sr = new StreamReader(ms, Encoding.UTF8)
          // This allows us to read a bunch of messages in from a single message
          while not sr.EndOfStream do
            let line = Ingested.ofString (sr.ReadLine())
            let nJ = job {
              match! next line with
              | Result.Error error ->
                config.ilogger.error error
              | Result.Ok () ->
                ()
              }
            queue nJ

      with :? ZMQError as zmq ->
        config.ilogger.warn("Shutting down ZMQ STREAM/TCP recv-loop due to {exn}. ZMQ Error No={errorNo}", fun m ->
          m.setField("exn", zmq.Message)
          m.setField("errorNo", zmq.ErrorNumber))
        reraise ()
    finally
      config.ilogger.info "Shutting down ZMQ STREAM/TCP recv-loop."
      queue (IVar.fill shutdown ())

  /// Create a new TCP ingestion server.
  let create: ServerFactory<TCPConfig> =
    IngestServer.create recv