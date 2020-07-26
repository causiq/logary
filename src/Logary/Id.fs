namespace Logary

open System
open System.Buffers

[<Struct>]
type Id =
  { high: int64
    low: int64 }

  member x.isZero = x.high = 0L && x.low = 0L

  static member Zero = { high=0L; low=0L }

  member x.toByteArray() =
    let h, l =
      BitConverter.GetBytes(x.high),
      BitConverter.GetBytes(x.low) // https://docs.microsoft.com/en-us/dotnet/api/system.bitconverter?view=netcore-3.1
    if BitConverter.IsLittleEndian then
      Array.Reverse(h) // macOS Catalina 64 is LE, convention: BE
      Array.Reverse(l)
    [| yield! h; yield! l |]

  member x.toBase64String() =
    let bs = x.toByteArray()
    Convert.ToBase64String(bs)

  static member ofBase64String (s: string) =
    use bs = MemoryPool.Shared.Rent(8)
    let mutable written = 0
    if Convert.TryFromBase64String(s, bs.Memory.Span, &written) && written = 16 then
      { high = BitConverter.ToInt64(Span<_>.op_Implicit (bs.Memory.Span.Slice(0, 8)))
        low = BitConverter.ToInt64(Span<_>.op_Implicit (bs.Memory.Span.Slice(8, 8))) }
    else
      Id.Zero

  member x.to32HexString() =
    String.Format("{0:x16}{1:x16}", x.high, x.low)

  override x.ToString() =
    if x.high = 0L then String.Format("{0:x16}", x.low)
    else String.Format("{0:x16}{1:x16}", x.high, x.low)

