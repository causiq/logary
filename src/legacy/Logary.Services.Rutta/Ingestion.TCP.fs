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
  { endpoint: IPEndPoint }
  static member create endpoint =
    { endpoint = endpoint }

let private ilogger = Log.create "Logary.Ingestion.TCP"

let streamRecvLoop (receiver: Socket) (next: Ingest) =
  let rec loop () =
    job {
      // https://gist.github.com/lancecarlson/fb0cfd0354005098d579
      // https://gist.github.com/claws/7231548#file-czmq-stream-server-c-L21
      try
        let frame = Socket.recv receiver
        // Aborted socket due to closing process:
        if isNull frame then
          return ()
        else
          let bs = Socket.recv receiver
          // http://api.zeromq.org/4-1:zmq-socket#toc19
          // A connection was made
          if bs.Length = 0 then
            return! loop ()
          else
            // printfn "Data received: %A" message
            use ms = new MemoryStream(bs)
            use sr = new StreamReader(ms, Encoding.UTF8)
            while not sr.EndOfStream do
              let line = Ingested.ofString (sr.ReadLine())
              let! res = next line
              ignore res

            ilogger.verbose (eventX "Looping...")
            return! loop ()

      with :? ZMQError as zmq ->
        ilogger.info (eventX "Shutting down streamRecvLoop due to {exn}." >> setField "exn" zmq.Message >> addExn zmq)
        return ()
  }

  loop ()

let create (config: TCPConfig) (next: Ingest) =
  Job.result ()