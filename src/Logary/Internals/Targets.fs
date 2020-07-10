namespace Logary.Internals

open Hopac
open Logary
open Logary.Internals
open Logary.Model

/// The protocol for a targets runtime path (not the shutdown).
type TargetMessage =
  /// Log and send something that can be ACK:ed with the message.
  | Log of message: LogaryMessageBase * ack: IVar<unit>
  /// Flush messages! Also, reply when you're done flushing your queue.
  | Flush of ack: IVar<unit> * nack: Promise<unit>

/// Logary's way to talk with Targets as seen from the Targets.
///
/// Targets are responsible for selecting over these channels in order to handle
/// shutdown and messages.
type TargetAPI =
  /// Gives you a way to perform internal logging and communicate with Logary.
  abstract runtime: RuntimeInfo
  /// A ring buffer that gives a Message to log and an ACK-IVar to signal after
  /// logging the message.
  abstract requests: RingBuffer<TargetMessage>
  /// A channel that the target needs to select on and then ACK once the target
  /// has fully shut down.
  abstract shutdownCh: Ch<IVar<unit>>

module Target =

  open Hopac.Infixes

  /// Adapts the logger to use as a target. In turn used by ILogger.External, when logging to an external Logary instance.
  let ofLogger (logger: Logger): TargetAPI -> Job<unit> =
    fun api ->
      let rec loop () =
        Alt.choose [
          api.shutdownCh ^=> fun ack -> IVar.fill ack ()
          RingBuffer.take api.requests ^=> function
            | Log (message, ack) ->
              logger.logWithAck(true, message) >>= function
                | Result.Ok _ ->
                  IVar.fill ack () >>= loop
                | Result.Error error ->
                  IVar.fillFailure ack (exn (error.ckind.ToString())) >>= loop
            | Flush (ack, _) ->
              IVar.fill ack () >>= loop
        ]
      loop () :> Job<unit>