namespace Logary.Internals

open Hopac
open Hopac.Infixes

type RingBuffer<'a> =
  private {
    putCh: Ch<'a>
    full: MVar<unit>
    empty: MVar<unit>
    takeCh: Ch<'a>
    takeBatchCh: Ch<uint16 * IVar<'a[]>>
  }

module internal Utils =
  open System
  let ringSizeValidate num =
    (num <> 0us) && ((num &&& (num-1us)) = 0us) && ((int num >>> 16) = 0)

module RingBuffer =
  open Utils

  let create (ringSize: uint16): Job<RingBuffer<'a>> =
    if not (ringSizeValidate ringSize) then
      failwith "ringSize must be a power of 2 and maximum ringSize can only be half the range of the index data types (uint16)"

    let self = { putCh = Ch (); full = MVar (); empty = MVar (); takeCh = Ch (); takeBatchCh = Ch () }
    let ring = Array.zeroCreate (int ringSize)
    let mutable read, write = 0us, 0us

    let inline mask x = int (x &&& (ringSize - 1us))

    let inline count () = write - read
    let inline empty () = read = write
    let inline full () = (int (count())) = ring.Length

    let inline enqueue x =
      ring.[mask write] <- x
      write <- write + 1us

    let inline dequeue () =
      read <- read + 1us

    let dequeueBatch (num, ivar) =
      let dequeueCount = min num <| count ()
      let arr = Array.zeroCreate (int dequeueCount)
      let maskRead = mask read
      let afterRead = read + dequeueCount
      let maskAfterRead = mask afterRead
      ivar *<=
        ( if maskRead < maskAfterRead then
            Array.blit ring maskRead arr 0 (int dequeueCount)
            read <- afterRead
            arr
          else
            let readToEndCount = int ringSize - maskRead
            let readFromStartCount = int dequeueCount - readToEndCount
            Array.blit ring maskRead arr 0 readToEndCount
            Array.blit ring 0 arr readToEndCount readFromStartCount
            read <- afterRead
            arr )

    let put () = self.putCh ^-> enqueue
    let take () = self.takeCh *<- ring.[mask read] ^-> dequeue
    let takeBatch () = self.takeBatchCh ^=> Job.delayWith dequeueBatch

    let proc = Job.delay <| fun () ->
      if empty () then
        MVar.fill self.empty ()
        >>= fun _ -> put ()
        >>= fun _ -> MVar.take self.empty
      elif full () then
        MVar.fill self.full ()
        >>= fun _ -> (takeBatch () <|> take ())
        >>= fun _ -> MVar.take self.full
      else (takeBatch () <|> take () <|> put ()) :> Job<_>

    Job.foreverServer proc >>-. self

  let put q x = q.putCh *<- x
  let tryPut q x = (q.putCh *<- x ^->. true) <|> (MVar.read q.full ^->. false)

  let inline private noneIfEmpty ring = MVar.read ring.empty ^->. None

  let take q = q.takeCh :> Alt<_>
  let tryTake q = (q.takeCh ^-> Some) <|> noneIfEmpty q

  let takeBatch (maxBatchSize : uint16) q = q.takeBatchCh *<-=>- (fun iv -> maxBatchSize, iv)
  let tryTakeBatch (maxBatchSize : uint16) q = (takeBatch maxBatchSize q ^-> Some) <|> noneIfEmpty q

  let takeAll q = takeBatch System.UInt16.MaxValue q
  let tryTakeAll q = (takeAll q ^-> Some) <|> noneIfEmpty q

  let consume q s = Stream.iterJob (fun x -> q.putCh *<- x) s |> Job.start
  let tap q = Stream.indefinitely <| q.takeCh
  let tapBatches maxBatchSize q = Stream.indefinitely <| takeBatch maxBatchSize q
  let tapAll q = Stream.indefinitely <| takeAll q
