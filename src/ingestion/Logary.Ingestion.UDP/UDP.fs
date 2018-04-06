module Logary.Ingestion.UDP

open Logary
open Logary.Message
open Logary.Internals
open Hopac
open Hopac.Infixes
open System
open System.Text
open System.Threading
open System.Net
open System.Net.Sockets

type UDPConfig =
    /// Where to listen.
  { endpoint: IPEndPoint
    /// Set this promise to a value to shut down the UDP receiver.
    cancelled: Promise<unit> }
  static member create ep cancelled =
    { endpoint = ep
      cancelled = cancelled }

let private ilogger = Log.create "Logary.Ingestion.UDP"

// https://docs.microsoft.com/en-us/dotnet/api/system.net.sockets.udpclient.receiveasync?view=netframework-4.7.1#System_Net_Sockets_UdpClient_ReceiveAsync
// exceptions not handled: https://msdn.microsoft.com/en-us/library/system.net.sockets.socketexception.errorcode(v=vs.110).aspx
let private handleDisposed recvThrew (xJ: Job<unit>) =
  Job.tryWith xJ (function
    // https://stackoverflow.com/a/18314614/63621
    | :? ObjectDisposedException -> recvThrew *<= ()
    | e -> Job.raises e)

type UdpClient with
  member internal x.getMessage () =
    Job.fromTask (fun () -> x.ReceiveAsync())
    |> Job.map (fun d -> d.Buffer)

/// Creates a new LogClient with an address, port and a sink (next.)
let create (config: UDPConfig) (next: Ingest) =
  let rec receiveLoop (inSocket: UdpClient) =
    job {
      let! msg = inSocket.getMessage()
      let! res = next (Ingested.ofBytes msg)
      ignore res // nothing to do; UDP is fire-and-forget
      return! receiveLoop inSocket
    }

  let recvThrew = IVar ()

  let receiveContext =
    job {
      do ilogger.info (eventX "Starting UDP log listener at {endpoint}" >> setField "endpoint" config.endpoint)
      use inSocket = new UdpClient(config.endpoint)
      try
        do! Job.start (handleDisposed recvThrew (receiveLoop inSocket))
        // if we're cancelled, fall through the finally and cancel the UdpClient. This will throw an
        // ObjectDisposedException on the parallel thread (Job.start-ed).
        // `handleDisposed` will set the flag `recvThrew` that we await after the receiveContext job
        // finishes.
        do! config.cancelled
      finally
        do ilogger.info (eventX "Stopping UDP log listener at {endpoint}" >> setField "endpoint" config.endpoint)
        try inSocket.Close()
        with _ -> ()
    }

  receiveContext >>=. recvThrew