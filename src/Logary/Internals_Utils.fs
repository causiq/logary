namespace Logary.Internals

module internal Date =
  open System
  open NodaTime
  let private epoch = new DateTime(1970, 1, 1, 0, 0, 0, 0, DateTimeKind.Utc)
  let toEpoch (dt : DateTime) = int64 <| (dt.ToUniversalTime() - epoch).TotalSeconds
  let utcNow () = SystemClock.Instance.Now

module internal Map =
  let put k v (m : Map<_,_>) =
    match m.TryFind k with
    | None -> m |> Map.add k v
    | Some _ -> m |> Map.remove k |> Map.add k v

[<AutoOpen>]
module internal Set =
  let (|EmptySet|_|) = function
    | (s : _ Set) when s.Count = 0 -> Some EmptySet
    | _ -> None

// TODO: this module can be redone much nicer with an expressive API of possible
// states rather then the current 'one method only' approach!

/// A module that wraps the .Net TcpClient types/streams in a F#-ideomatic way,
/// and behind an interface.
module Tcp =

  open System

  type WriteStream =
    /// WriteAsync : buffer -> offset? -> length?
    abstract Write : byte array -> unit Async

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
