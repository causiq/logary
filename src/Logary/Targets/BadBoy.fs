/// A module to test how your app reacts to logging to a very slow target.
module Logary.Targets.BadBoy

open Hopac
open Hopac.Infixes
open Logary
open Logary.Message
open Logary.Internals
open Logary.Configuration.Target
open NodaTime

type BadBoyConf =
  { /// Delay every message / every message batch (if batching) this much
    delay: Duration
    batch: bool }

/// Defaults: batch=true, delay=200ms
let empty =
  { delay = Duration.FromMilliseconds 200L
    batch = true }

module internal Impl =
  let loop (conf: BadBoyConf) (api: TargetAPI): Job<_> =
    let ilogger = api.runtime.logger

    let rec singleLoopDelay () =
      let take =
        RingBuffer.take api.requests ^=> function
          | Log (message, ack) ->
            ilogger.timeAlt (timeOut (conf.delay.ToTimeSpan()), "single loop delay")
            >>=. ack *<= ()
            >>= singleLoopDelay
          | Flush (ack, nack) ->
            let simulateWrite = timeOut (conf.delay.ToTimeSpan()) ^=>. IVar.fill ack ()
            (simulateWrite <|> nack) >>= singleLoopDelay

      let shutdown =
        api.shutdownCh ^=> fun ack -> IVar.fill ack ()

      take <|> shutdown

    let rec singleLoop () =
      let take =
        RingBuffer.take api.requests ^=> function
          | Log (message, ack) -> IVar.fill ack () >>= singleLoop
          | Flush (ack, _) -> IVar.fill ack () >>= singleLoop

      let shutdown =
        api.shutdownCh ^=> fun ack -> IVar.fill ack ()

      take <|> shutdown

    let rec batchLoopDelay () =
      let take =
        RingBuffer.takeBatch 256us api.requests ^=> fun messages ->
          let entries, acks, flushes =
            // Make a single pass over the array, accumulating the entries, acks and flushes.
            messages |> Array.fold (fun (entries, acks, flushes) -> function
              | Log (message, ack) ->
                message :: entries,
                ack *<= () :: acks,
                flushes
              | Flush (ackCh, nack) ->
                entries,
                acks,
                ackCh *<= () :: flushes)
              ([], [], [])

          ilogger.timeAlt (timeOut (conf.delay.ToTimeSpan()), "batch loop delay, delaying", logBefore=true) ^=> fun () ->
          Job.conIgnore acks >>=. Job.conIgnore flushes >>= batchLoopDelay

      let shutdown =
        api.shutdownCh ^=> fun ack -> IVar.fill ack ()

      ilogger.timeAlt (take, "batch loop delay, take", logBefore=true) <|> shutdown

    let batchLoop () =
      let take =
        RingBuffer.takeBatch 256us api.requests ^=> fun messages ->
          let entries, acks, flushes =
            // Make a single pass over the array, accumulating the entries, acks and flushes.
            messages |> Array.fold (fun (entries, acks, flushes) -> function
              | Log (message, ack) ->
                message :: entries,
                ack *<= () :: acks,
                flushes
              | Flush (ackCh, nack) ->
                entries,
                acks,
                ackCh *<= () :: flushes)
              ([], [], [])
          Job.conIgnore acks >>=. Job.conIgnore flushes >>= batchLoopDelay

      let shutdown =
        api.shutdownCh ^=> fun ack -> IVar.fill ack ()

      ilogger.timeAlt (take, "batch loop, take", logBefore=true) <|> shutdown

    api.runtime.logger.debug (
      eventX "Starting BadBoy with delay={delay}, batch={batch}."
      >> setFieldsFromObject conf)

    if conf.batch then
      if conf.delay <> Duration.Zero then
        upcast batchLoopDelay ()
      else
        upcast batchLoop ()
    else
      if conf.delay <> Duration.Zero then
        upcast singleLoopDelay ()
      else
        upcast singleLoop ()

[<CompiledName "Create">]
let create conf name = TargetConf.createSimple (Impl.loop conf) name

type Builder(conf, callParent: ParentCallback<Builder>) =
  let update (conf': BadBoyConf): Builder = Builder(conf', callParent)
  member x.NoBatch() = update { conf with batch = false }
  member x.Delay(delay: Duration) = update { conf with delay = delay }
  new(callParent: ParentCallback<_>) = Builder(empty, callParent)
  interface SpecificTargetConf with member x.Build name = create conf name