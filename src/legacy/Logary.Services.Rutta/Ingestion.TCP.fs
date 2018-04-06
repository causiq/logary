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
    receiver: Socket
  }
  static member create socket cancelled =
    { cancelled = cancelled
      receiver = socket
    }

let private ilogger = Log.create "Logary.Ingestion.TCP"

let recv (conf: TCPConfig) (next: Ingest) =
  // https://gist.github.com/lancecarlson/fb0cfd0354005098d579
  // https://gist.github.com/claws/7231548#file-czmq-stream-server-c-L21
  job {
    try
      while not (Promise.Now.isFulfilled conf.cancelled) do
        let frame = Socket.recv conf.receiver
        // Aborted socket due to closing process:
        if isNull frame then () else
        let bs = Socket.recv conf.receiver
        // http://api.zeromq.org/4-1:zmq-socket#toc19
        // A connection was made
        if bs.Length = 0 then () else
        // printfn "Data received: %A" message
        use ms = new MemoryStream(bs)
        use sr = new StreamReader(ms, Encoding.UTF8)
        while not sr.EndOfStream do
          let line = Ingested.ofString (sr.ReadLine())
          let! res = next line
          ignore res
        ilogger.verbose (eventX "Looping...")
    with :? ZMQError as zmq ->
      ilogger.info (eventX "Shutting down streamRecvLoop due to {exn}." >> setField "exn" zmq.Message)
  }

let create (config: TCPConfig) (next: Ingest) =
  Job.start (recv config next)
