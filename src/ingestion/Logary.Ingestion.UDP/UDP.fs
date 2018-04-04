module Logary.Ingestion.UDP

open Logary
open Logary.Message
open Hopac
open System
open System.Net
open System.Net.Sockets

type UDPConfig =
  { endpoint: IPEndPoint
    next: byte[] -> Job<unit> }

let private ilogger = Log.create "LogClient"

let private handleDisposed (xJ: Job<unit>) =
  Job.tryWith xJ (function
    // https://stackoverflow.com/a/18314614/63621
    | :? ObjectDisposedException -> Job.result ()
    | e -> Job.raises e)

type UdpClient with
  member internal x.getMessage () =
    Job.fromTask (fun () -> x.ReceiveAsync())
    |> Job.map (fun d -> d.Buffer)

/// Creates a new LogClient with an address, port and a sink (next.)
let create (config: UDPConfig) =
  let rec receiveLoop (inSocket: UdpClient) =
    job {
      let! msg = inSocket.getMessage()
      do! config.next msg
      return! receiveLoop inSocket
    }

  job {
    do ilogger.info (eventX "Starting UDP log listener at {endpoint}" >> setField "endpoint" config.endpoint)
    use inSocket = new UdpClient(config.endpoint)
    try
      do! handleDisposed (receiveLoop inSocket)
    finally
      do ilogger.info (eventX "Stopping UDP log listener at {endpoint}" >> setField "endpoint" config.endpoint)
      try inSocket.Close()
      with _ -> ()
  }

  // https://docs.microsoft.com/en-us/dotnet/api/system.net.sockets.udpclient.receiveasync?view=netframework-4.7.1#System_Net_Sockets_UdpClient_ReceiveAsync
  // exceptions not handled: https://msdn.microsoft.com/en-us/library/system.net.sockets.socketexception.errorcode(v=vs.110).aspx