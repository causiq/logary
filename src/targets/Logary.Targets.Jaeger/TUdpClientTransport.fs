namespace Logary.Targets

open Thrift.Transports
open System
open System.Threading.Tasks
open System.Net.Sockets

type TUdpClientTransport(udpClient: UdpClient, ?hostName, ?port: int) =
  inherit TClientTransport()

  let mutable disposed = false

  new (hostName: string, port: int) =
    let udpClient = new UdpClient(hostName, port)
    new TUdpClientTransport(udpClient, hostName, port)

  override x.IsOpen =
    Option.ofObj udpClient
    |> Option.bind (fun c -> Option.ofObj c.Client)
    |> Option.map (fun c -> c.Connected)
    |> Option.defaultValue false

  override x.OpenAsync ct =
    if ct.IsCancellationRequested then Task.FromCanceled ct
    else Task.CompletedTask

  override x.ReadAsync (_, _, _, _) =
    // since agent here https://github.com/jaegertracing/jaeger-idl/blob/master/thrift/agent.thrift#L24-L25
    // is one way communication, so not implemented here
    raise (new NotImplementedException("The TUdpClientTransport cannot read UDP datagrams, only send them. Programming error."))

  override x.WriteAsync (buffer, offset, length, ct) =
    if ct.IsCancellationRequested then Task.FromCanceled ct
    else
      try
        let realData = buffer.AsMemory().Slice(offset,length).ToArray()
        udpClient.SendAsync(realData, realData.Length).ContinueWith(fun (_: Task<int>) -> do ())
      with :? SocketException as se ->
        let h = Option.defaultValue "<UdpClient>" hostName
        let p = Option.defaultValue "<port>" (Option.map string port)
        let im =
          match se.SocketErrorCode with
          | SocketError.ConnectionRefused -> " Connection was refused, which indicate nothing is listening on this IP:PORT pair. You either haven't started your Jaeger agent service or it's not accessible from this host."
          | _ -> ""
        raise (Exception(sprintf "Transport failed send to udp://%s:%s.%s" h p im, se))

  override x.FlushAsync ct =
    if ct.IsCancellationRequested then Task.FromCanceled ct
    else Task.CompletedTask

  override x.Close () = udpClient.Close ()

  override x.Dispose disposing =
    if not disposed && disposing then
      udpClient.Dispose()
      disposed <- true
    else do ()



