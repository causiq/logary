namespace Logary.Internals

module internal Date =
  open System
  open NodaTime
  let private epoch = new DateTime(1970, 1, 1, 0, 0, 0, 0, DateTimeKind.Utc)
  let toEpoch (dt : DateTime) = int64 <| (dt.ToUniversalTime() - epoch).TotalSeconds
  let utcNow () = SystemClock.Instance.Now

/// A module that wraps the .Net TcpClient types/streams in a F#-ideomatic
/// way, and behind an interface.
module Tcp =

  open System

  type WriteStream =
    /// WriteAsync : buffer -> offset? -> length?
    abstract Write : byte array -> unit Async

  type WriteClient =
    inherit IDisposable
    abstract GetStream : unit -> WriteStream

  open System.Net.Sockets

  type TcpWriteStream(ns : NetworkStream) =
    let orDefault = Option.fold (fun s t -> t)
    interface WriteStream with
      member x.Write buffer =
        let offset, len = Some 0, Some(buffer.Length)
        ns.AsyncWrite(buffer,
          offset |> orDefault 0,
          len    |> orDefault buffer.Length)
    interface IDisposable with
      member x.Dispose () = (ns :> IDisposable).Dispose()

  type TcpWriteClient(tcpClient : TcpClient) =
    interface WriteClient with
      member x.GetStream () =
        new TcpWriteStream(tcpClient.GetStream()) :> WriteStream
    interface IDisposable with
      member x.Dispose () = (tcpClient :> IDisposable).Dispose()
