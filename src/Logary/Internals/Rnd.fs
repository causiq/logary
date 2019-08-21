namespace Logary.Internals

module internal Rnd =
  open System
  open System.Threading

  let private BitsPerLong = 63

  let random = new Threading.ThreadLocal<_>(fun () -> Random())

  let nextInt () =
    random.Value.Next (Int32.MinValue, Int32.MaxValue)

  /// Get the next int within [0, max]
  let nextIntMax max =
    random.Value.Next max

  let nextInt64 () =
    let buf = Array.zeroCreate sizeof<int64>
    random.Value.NextBytes buf
    BitConverter.ToInt64 (buf, 0)

  let nextInt64NonZero () =
    let mutable value = 0L
    while value = 0L do
      value <- nextInt64 ()
    value

  /// Get the next int64 within [0, max]
  let nextInt64Max (max: int64) =
    let mutable bits = 0L
    let mutable value = 0L
    let mutable first = true
    while first || bits - value + (max - 1L) < 0L do
      bits <- nextInt64 () &&& (~~~(1L <<< BitsPerLong))
      value <- bits % max
      first <- false
    value

