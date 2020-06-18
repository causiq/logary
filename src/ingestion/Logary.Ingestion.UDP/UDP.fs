namespace Logary.Ingestion

open Logary
open Logary.Internals
open Hopac
open Hopac.Infixes
open System
open System.Net
open System.Net.Sockets

type UDPConfig =
  { /// Set this promise to a value to shut down the UDP receiver.
    cancelled: Promise<unit>
    /// Where to listen.
    bindings: BindingList
    ilogger: Logger }

  interface IngestServerConfig with
    member x.cancelled = x.cancelled
    member x.ilogger = x.ilogger
    member x.bindings = x.bindings

  static member create bindings cancelled ilogger =
    { cancelled = cancelled
      bindings = bindings
      ilogger = ilogger }

module internal Impl =
  let hasODE (ae: AggregateException) =
    let ae = ae.Flatten()
    ae.InnerExceptions
    |> Seq.tryPick (function
      | :? ObjectDisposedException as ode -> Some ode
      | _ -> None)

  type UdpClient with
    member x.getMessage () =
      let xJ =
        Job.fromTask (fun () -> x.ReceiveAsync())
        |> Job.map (fun d -> d.Buffer)

      Job.tryWith xJ (function
        | :? AggregateException as ae ->
          match hasODE ae with
          | Some ode ->
            Job.raises ode
          | None ->
            Job.raises ae
        | e ->
          Job.raises e)

  /// https://docs.microsoft.com/en-us/dotnet/api/system.net.sockets.udpclient.receiveasync?view=netframework-4.7.1#System_Net_Sockets_UdpClient_ReceiveAsync
  /// exceptions not handled: https://msdn.microsoft.com/en-us/library/system.net.sockets.socketexception.errorcode(v=vs.110).aspx
  /// https://stackoverflow.com/a/18314614/63621
  let ignoreODE completedIV (xJ: Job<unit>) =
    Job.tryWith xJ (function
      | :? ObjectDisposedException ->
        IVar.fill completedIV ()
      | e ->
        IVar.fillFailure completedIV e)

  //[<TailRecursive>]
  /// Iterate over the UDP datagram inputs, interpreting them as bytes, send them onto `next`, take the next datagram.
  /// Then, queue it all up as a Proc, to allow us to join it later.
  let receiveLoop (ilogger: Logger) (next: Ingest) (inSocket: UdpClient) =
    job {
      while true do
        let! msg = inSocket.getMessage()
        match! next (Ingested.ofBytes msg) with
        | Result.Error error ->
          ilogger.error error
        | Result.Ok () ->
          ()
    }

module UDP =
  let internal addressFrom (endpoint: IPEndPoint) =
    let a = endpoint.Address
    if a.Equals IPAddress.Any then
      IPAddress.Loopback
    elif a.Equals IPAddress.IPv6Any then
      IPAddress.IPv6Loopback
    else
      a

  let recvSingle (ilogger: Logger, onStartJ: Job<unit>, onShutdownJ: Job<unit>, cancelled: Promise<unit>, endpoint: IPEndPoint) (ingest: Ingest) =
    let ilogger = ilogger |> Logger.apply (fun m -> m.setField("endpoint", endpoint.ToString()))
    job {
      ilogger.info(
        "Starting UDP recv-loop at {endpoint}. You can send data to this endpoint with {ncCommand}, and then typing your message/JSON. The UDP target expects datagrams, so the newline character will be part of the message if you send those.",
        fun m -> m.setField("ncCommand", sprintf "nc -u -p 54321 %O %i" endpoint endpoint.Port))

      let completed = IVar ()
      use inSocket = new UdpClient(endpoint)

      let close () =
        ilogger.info "Stopping UDP recv-loop at {endpoint}"
        try inSocket.Close() with _ -> ()

      // start the client "server" Proc
      do! Job.start (Impl.ignoreODE completed (Impl.receiveLoop ilogger ingest inSocket))
      do! onStartJ

      // this may throw, thereby ignoring the wait on the `cancelled` Promise
      return!
        Job.tryFinallyJob
          (Job.tryFinallyFun (cancelled <|> completed) close)
          onShutdownJ
    }

  let recv (started: IVar<unit>, shutdown: IVar<unit>) (config: UDPConfig) (ingest: Ingest) =
    let startLatch, shutdownLatch =
      Latch config.bindings.Length,
      Latch config.bindings.Length

    let startAllJ =
      config.bindings
        |> Seq.map (fun binding ->
          recvSingle (config.ilogger,
                      Latch.decrement startLatch,
                      Latch.decrement shutdownLatch,
                      config.cancelled,
                      binding.asEndpoint)
                     ingest)
        |> Job.conIgnore
        |> Job.start

    job {
      do! startAllJ
      do! Latch.await startLatch ^=> IVar.fill started
      do! Latch.await shutdownLatch ^=> IVar.fill shutdown
    }

  /// Creates a new LogClient with an address, port and a sink (next.)
  let create: ServerFactory<UDPConfig> =
    IngestServer.create recv