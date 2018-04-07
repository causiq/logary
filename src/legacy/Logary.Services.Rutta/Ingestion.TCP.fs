module Logary.Ingestion.TCP


open System.Text
open System.IO
open System.Net
open Logary
open Logary.Message
open Logary.Formatting
open Logary.Internals
open Logary.Internals.Chiron
open Hopac
open fszmq

type TCPConfig =
  { cancelled: Promise<unit>
    createSocket: Context -> Socket
    ilogger: Logger
  }
  static member create createSocket cancelled ilogger =
    { cancelled = cancelled
      createSocket = createSocket
      ilogger = ilogger
    }

let recv (conf: TCPConfig) (next: Ingest) =
  // https://gist.github.com/lancecarlson/fb0cfd0354005098d579
  // https://gist.github.com/claws/7231548#file-czmq-stream-server-c-L21
    try
      use context = new Context()
      use socket = conf.createSocket context
      while not (Promise.Now.isFulfilled conf.cancelled) do
        // https://github.com/zeromq/libzmq/issues/1573
        let frame = Socket.recv socket
        // Aborted socket due to closing process:
        if isNull frame then () else
        // https://github.com/zeromq/libzmq/issues/1573
        let bs = Socket.recv socket
        // http://api.zeromq.org/4-1:zmq-socket#toc19
        // A connection was made
        if bs.Length = 0 then () else
        use ms = new MemoryStream(bs)
        use sr = new StreamReader(ms, Encoding.UTF8)
        while not sr.EndOfStream do
          let line = Ingested.ofString (sr.ReadLine())
          let nJ = next line
          start (Job.Ignore nJ)
        conf.ilogger.verbose (eventX "Looping...")
    with :? ZMQError as zmq ->
      conf.ilogger.info (eventX "Shutting down recv-loop due to {exn}." >> setField "exn" zmq.Message)

let create (config: TCPConfig) (next: Ingest) =
  let iJ = Job.Scheduler.isolate (fun () -> recv config next)
  Job.queue iJ