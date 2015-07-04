namespace Logary.Internals

open System
open System.IO
// DEPRECATED, refactor!

// TODO: this module can be redone much nicer with an expressive API of possible
// states rather then the current 'one method only' approach!

/// A module that wraps the .Net TcpClient types/streams in a F#-ideomatic way,
/// and behind an interface.
module Tcp =
  /// Transfer the said amount from `source` stream to `target` stream
  let transfer len (source : Stream) (target : Stream) =
    let bufSize = 0x2000
    let buf = Array.zeroCreate bufSize // TODO: extract
    let rec read' amountRead = async {
      if amountRead >= len then
        return ()
      else
        let toRead = Math.Min(bufSize, len - amountRead)
        let! wasRead = source.AsyncRead(buf, 0, toRead)
        if wasRead <> toRead then
          raise <| EndOfStreamException(sprintf "unexpected EOF (wasRead: %d, toRead: %d)" wasRead toRead)
        else
          do! target.AsyncWrite(buf, 0, wasRead)
          return! read' (wasRead + amountRead) }
    read' 0

  type WriteStream =
    /// WriteAsync : buffer -> offset? -> length?
    abstract Write : byte array -> Async<unit>

  type WriteClient =
    inherit IDisposable
    /// Gets a stream for this write client
    abstract GetStream : unit -> WriteStream

  open System.IO
  open System.Net.Sockets
  open System.Net.Security
  open System.Security.Cryptography.X509Certificates

  type TcpWriteStream(stream : Stream) =
    let orDefault = Option.fold (fun s t -> t)
    interface WriteStream with
      member x.Write buffer =
        let offset, len = Some 0, Some(buffer.Length)
        stream.AsyncWrite(buffer,
          offset |> orDefault 0,
          len    |> orDefault buffer.Length)
    interface IDisposable with
      member x.Dispose () =
        (stream :> IDisposable).Dispose()

  type TLSWriteClient(tcpClient    : TcpClient,
                      certCallback : X509Certificate -> X509Chain -> SslPolicyErrors -> bool) =
    interface WriteClient with
      member x.GetStream () =
        let cb = new RemoteCertificateValidationCallback(fun _ -> certCallback)
        let sslStream = new SslStream(tcpClient.GetStream(), false, cb)
        new TcpWriteStream(sslStream):> WriteStream

    interface IDisposable with
      member x.Dispose () =
        (tcpClient :> IDisposable).Dispose()

  type TcpWriteClient(tcpClient : TcpClient) =
    interface WriteClient with
      member x.GetStream () =
        let stream = tcpClient.GetStream()
        new TcpWriteStream(stream) :> WriteStream

    interface IDisposable with
      member x.Dispose () =
        (tcpClient :> IDisposable).Dispose()
