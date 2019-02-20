namespace Logary.Prometheus


#nowarn "9"

/// port from java(jdk) DoubleAdder
/// - https://github.com/JetBrains/jdk8u_jdk/blob/master/src/share/classes/java/util/concurrent/atomic/Striped64.java
/// - https://github.com/prometheus/client_java/blob/master/simpleclient/src/main/java/io/prometheus/client/Striped64.java
///    /*
///     * This class maintains a lazily-initialized table of atomically
///     * updated variables, plus an extra "base" field. The table size
///     * is a power of two. Indexing uses masked per-thread hash codes.
///     * Nearly all declarations in this class are package-private,
///     * accessed directly by subclasses.
///     *
///     * Table entries are of class Cell; a variant of AtomicLong padded
///     * to reduce cache contention on most processors. Padding is
///     * overkill for most Atomics because they are usually irregularly
///     * scattered in memory and thus don't interfere much with each
///     * other. But Atomic objects residing in arrays will tend to be
///     * placed adjacent to each other, and so will most often share
///     * cache lines (with a huge negative performance impact) without
///     * this precaution.
///     *
///     * In part because Cells are relatively large, we avoid creating
///     * them until they are needed.  When there is no contention, all
///     * updates are made to the base field.  Upon first contention (a
///     * failed CAS on base update), the table is initialized to size 2.
///     * The table size is doubled upon further contention until
///     * reaching the nearest power of two greater than or equal to the
///     * number of CPUS. Table slots remain empty (null) until they are
///     * needed.
///     *
///     * A single spinlock ("busy") is used for initializing and
///     * resizing the table, as well as populating slots with new Cells.
///     * There is no need for a blocking lock; when the lock is not
///     * available, threads try other slots (or the base).  During these
///     * retries, there is increased contention and reduced locality,
///     * which is still better than alternatives.
///     *
///     * Per-thread hash codes are initialized to random values.
///     * Contention and/or table collisions are indicated by failed
///     * CASes when performing an update operation (see method
///     * retryUpdate). Upon a collision, if the table size is less than
///     * the capacity, it is doubled in size unless some other thread
///     * holds the lock. If a hashed slot is empty, and lock is
///     * available, a new Cell is created. Otherwise, if the slot
///     * exists, a CAS is tried.  Retries proceed by "double hashing",
///     * using a secondary hash (Marsaglia XorShift) to try to find a
///     * free slot.
///     *
///     * The table size is capped because, when there are more threads
///     * than CPUs, supposing that each thread were bound to a CPU,
///     * there would exist a perfect hash function mapping threads to
///     * slots that eliminates collisions. When we reach capacity, we
///     * search for this mapping by randomly varying the hash codes of
///     * colliding threads.  Because search is random, and collisions
///     * only become known via CAS failures, convergence can be slow,
///     * and because threads are typically not bound to CPUS forever,
///     * may not occur at all. However, despite these limitations,
///     * observed contention rates are typically low in these cases.
///     *
///     * It is possible for a Cell to become unused when threads that
///     * once hashed to it terminate, as well as in the case where
///     * doubling the table causes no thread to hash to it under
///     * expanded mask.  We do not try to detect or remove such cells,
///     * under the assumption that for long-running instances, observed
///     * contention levels will recur, so the cells will eventually be
///     * needed again; and for short-lived ones, it does not matter.
///     */
[<AutoOpen>]
module DoubleAdder =
  open System
  open System
  open System
  open System.Threading
  open System.Runtime.InteropServices;


  let CPU_CORE_NUM = Environment.ProcessorCount
  let mutable private HashCodeProbe = 0

  /// cell
  [<StructLayout(LayoutKind.Explicit, Size = 64, CharSet = CharSet.Ansi)>]
  type internal Cell =
    [<FieldOffset(0)>]
    val mutable content: int64

    new(x) = { content = x }

    member x.tryAdd value =
      let contentLocal = x.content
      let addedValue = BitConverter.Int64BitsToDouble contentLocal + value |> BitConverter.DoubleToInt64Bits
      contentLocal = Interlocked.CompareExchange(&x.content, addedValue, contentLocal)


  [<StructLayout(LayoutKind.Explicit, Size = 88, CharSet = CharSet.Ansi)>]
  type DoubleAdder =
    [<FieldOffset(0)>]
    val mutable private  baseValue: int64
    [<FieldOffset(64)>]
    [<DefaultValue>]
    val mutable private cellBusy: int
    [<FieldOffset(72)>]
    [<DefaultValue>]
    val mutable private cells: Cell[]

    [<FieldOffset(80)>]
    val mutable private threadHashCode: ThreadLocal<int ref>

    new (x: float) = {
      baseValue = BitConverter.DoubleToInt64Bits x;
      threadHashCode = new ThreadLocal<int ref>(fun _ ->
        let a = Interlocked.Add(&HashCodeProbe, 0x9e3779b9)
        ref (if a = 0 then 1 else a));
    }

    new () = new DoubleAdder(0.)

    member x.tryLockCellBusy () =
      x.cellBusy = 0 && 0 = Interlocked.CompareExchange(&x.cellBusy, 1, 0)

    member x.Add (value: float) =
      let mutable stillLoop = true
      let mutable threadHashCodeRef: int ref = Unchecked.defaultof<_>

      let mutable collideFlip = false

      while (stillLoop) do
        let cellsLocal = x.cells

        if isNull cellsLocal then
          let baseValueLocal = x.baseValue
          let addedValue = BitConverter.Int64BitsToDouble baseValueLocal + value |> BitConverter.DoubleToInt64Bits
          if (baseValueLocal = Interlocked.CompareExchange(&x.baseValue, addedValue, baseValueLocal)) then
            stillLoop <- false
          else
            // create cells
            if (isNull x.cells && x.tryLockCellBusy()) then
              try
                if (isNull x.cells) then
                  let cs = Array.zeroCreate<Cell> 2
                  cs.[0] <- new Cell(BitConverter.DoubleToInt64Bits value)
                  Volatile.Write(&x.cells, cs)
                  stillLoop <- false
              finally
                x.cellBusy <- 0
        else
          // hash to cell to add
          // 1. get hash if none, create it
          // 2. hash to cell index, if cell null, create, if not null , cas its value
          if (isNull (box threadHashCodeRef)) then
            threadHashCodeRef <- x.threadHashCode.Value

          let cellsLocal = x.cells
          let cellsLocalLen = cellsLocal.Length
          let mutable hashCode = threadHashCodeRef.Value
          let cell = cellsLocal.[(cellsLocalLen - 1) &&& hashCode]
          let mutable needRehash = false

          if (isNull (box cell)) then
            // create cell
            if(x.tryLockCellBusy()) then
              try
                if (cellsLocal = x.cells) then
                  let cellToBeAttached = new Cell(BitConverter.DoubleToInt64Bits value)
                  cellsLocal.[(cellsLocalLen - 1) &&& hashCode] <- cellToBeAttached
                  stillLoop <- false
                else
                  needRehash <- true
                  collideFlip <- false
              finally
                x.cellBusy <- 0
            else
              needRehash <- true
              collideFlip <- false
          else
            // cell not null, try add value in cell
            if(cell.tryAdd value) then
              stillLoop <- false
            else
              // faile to cas cell, rehash and retry
              if (cellsLocalLen >= CPU_CORE_NUM) then
                collideFlip <- false // no collide op(expand cells) can be done, set flip false to retry(skip expand operation)
                needRehash <- true
              else
                if (collideFlip) then
                  collideFlip <- true // set flip true,means rehash and retry if fails (cell cas) again, expand cells
                  needRehash <- true
                else
                  // expand cells
                  if (cellsLocal = x.cells && x.tryLockCellBusy()) then
                      try
                        if (cellsLocal = x.cells) then
                          let cellsExpanded = Array.zeroCreate<Cell> (cellsLocalLen <<< 1)
                          Array.iteri (fun oldCellIdx oldCell -> cellsExpanded.[oldCellIdx] <- oldCell) cellsLocal
                          cellsExpanded.[cellsLocalLen] <- new Cell(BitConverter.DoubleToInt64Bits value)
                          Volatile.Write(&x.cells, cellsExpanded)
                          stillLoop <- false
                        else needRehash <- true
                      finally
                        x.cellBusy <- 0
                  else needRehash <- true

            if needRehash then
              hashCode <- hashCode ^^^ (hashCode <<< 13)
              hashCode <- hashCode ^^^ (hashCode >>> 17)
              hashCode <- hashCode ^^^ (hashCode <<< 5)
              threadHashCodeRef := hashCode

    /// Returns the current sum.  The returned value is **NOT** an
    /// atomic snapshot; invocation in the absence of concurrent
    /// updates returns an accurate result, but concurrent updates that
    /// occur while the sum is being calculated might not be
    /// incorporated.  Also, because floating-point arithmetic is not
    /// strictly associative, the returned result need not be identical
    /// to the value that would be obtained in a sequential series of
    /// updates to a single variable.
    member x.Sum () =
      let mutable sum = BitConverter.Int64BitsToDouble x.baseValue
      let cells = x.cells

      match cells with
      | null -> ()
      | _ ->
        for cell in cells do
          match box cell with
          | null -> ()
          | _ -> sum <- sum + BitConverter.Int64BitsToDouble cell.content

      sum

    /// Resets variables maintaining the sum to zero.  This method may
    /// be a useful alternative to creating a new adder, but is only
    /// effective if there are no concurrent updates.  Because this
    /// method is intrinsically racy, it should only be used when it is
    /// known that no threads are concurrently updating.
    member x.Reset () =
      Volatile.Write(&x.baseValue, 0L)
      let cellsnapshot = x.cells
      match cellsnapshot with
      | null -> ()
      | _ ->
        for cell in cellsnapshot do
          match box cell with
          | null -> ()
          | _ -> cell.content <- 0L

