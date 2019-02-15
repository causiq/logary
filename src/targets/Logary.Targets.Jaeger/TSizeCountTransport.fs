namespace Logary.Clients.Jaeger

open Thrift.Transports
open System.Threading.Tasks
open System

type TSizeCountTransport() =
  inherit TClientTransport()

  let mutable size = 0
  override x.IsOpen = true

  override x.OpenAsync ct = 
    if ct.IsCancellationRequested then Task.FromCanceled ct
    else Task.CompletedTask

  override x.ReadAsync (buffer, offset, length,  ct) =
    // since agent here https://github.com/jaegertracing/jaeger-idl/blob/master/thrift/agent.thrift#L24-L25
    // is one way communication, so not implemented here
    raise (new NotImplementedException("size count no need to read"))

  override x.WriteAsync (buffer, offset, length,  ct) =
    if ct.IsCancellationRequested then Task.FromCanceled ct
    else
      size <- size + length
      Task.CompletedTask

  override x.FlushAsync ct =
    if ct.IsCancellationRequested then Task.FromCanceled ct
    else Task.CompletedTask

  override x.Close () = do ()

  override x.Dispose disposing = do ()

  member x.Size = size
