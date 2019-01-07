namespace Logary.Targets.Jaeger

open Thrift.Transports
open System.Net.Sockets
open System.Threading.Tasks
open System

type TUdpClientTransport(udpClient: UdpClient) =
  inherit TClientTransport()

  let mutable disposed = false

  new (hostName: string, port: int) =
    let udpClient = new UdpClient(hostName, port)
    new TUdpClientTransport(udpClient)

  override x.IsOpen =
    Option.ofObj udpClient
    |> Option.bind (fun c -> Option.ofObj c.Client)
    |> Option.map (fun c -> c.Connected)
    |> Option.defaultValue false

  override x.OpenAsync ct = 
    if ct.IsCancellationRequested then Task.FromCanceled ct
    else Task.CompletedTask

  override x.ReadAsync (buffer, offset, length,  ct) =
    // since agent here https://github.com/jaegertracing/jaeger-idl/blob/master/thrift/agent.thrift#L24-L25
    // is one way communication, so not implemented here
    raise (new NotImplementedException("udp client here only for one way (sending message to server)"))

  override x.WriteAsync (buffer, offset, length,  ct) =
    if ct.IsCancellationRequested then Task.FromCanceled ct
    else
      let realData = buffer.AsMemory().Slice(offset,length).ToArray()
      udpClient.SendAsync(realData, realData.Length).ContinueWith(fun (_: Task<int>) -> do ())

  override x.FlushAsync ct =
    if ct.IsCancellationRequested then Task.FromCanceled ct
    else Task.CompletedTask

  override x.Close () = udpClient.Close ()

  override x.Dispose disposing = 
    if not disposed && disposing then 
      udpClient.Dispose()
      disposed <- true
    else do ()



  