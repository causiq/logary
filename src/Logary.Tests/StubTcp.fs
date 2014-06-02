module StubTcp
  open System
  open System.IO
  open System.Text

  open Logary
  open Logary.Internals
  open Tcp

  type StubWriterClient(ignoreDispose : bool) =
    let ms = new MemoryStream()
    let wasDisposed = ref false

    member x.WasDisposed = !wasDisposed
    member x.ReadLines () =
      seq {
        ms.Seek(0L, SeekOrigin.Begin) |> ignore
        use sr = new StreamReader(ms)
        while not <| sr.EndOfStream do
          yield sr.ReadLine() }
    override x.ToString() = Encoding.UTF8.GetString(ms.ToArray())

    interface Tcp.WriteClient with
      member x.GetStream () =
        new StubWriterStream(ms) :> Tcp.WriteStream

    interface IDisposable with
      member x.Dispose () =
        wasDisposed := true
        if not ignoreDispose then ms.Dispose()

    static member Create host port =
      new StubWriterClient(true) :> Tcp.WriteClient

  and StubWriterStream(ms) =
    let wasDisposed = ref false

    member x.WasDisposed = !wasDisposed

    interface Tcp.WriteStream with
      member x.Write buf = async {
        ms.Write(buf, 0, buf.Length)
        return () }

    interface IDisposable with
      member x.Dispose() =
        wasDisposed := true
        ()
