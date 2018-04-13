namespace Logary

open System
open Logary.Internals
open Hopac

type IDataSlot =
  abstract collect: unit -> obj list
  abstract push: Lazy<obj> -> IDisposable
  abstract wrap: IDataSlot -> unit


type SpanIdGenerator =
  private {
    runtime: RuntimeInfo
    mutable counter: int
  }

  member x.Generate () =
    let prefixWithTime = sprintf "%s-%s.%x.%d" x.runtime.host x.runtime.service
    let prefix = prefixWithTime (x.runtime.getTimestamp ())

    lock (prefixWithTime) (fun _ ->
      x.counter <- x.counter + 1
      if x.counter < 0 then
        x.counter <- 0
        prefixWithTime (x.runtime.getTimestamp ()) 0
      else
        prefix x.counter
    )

/// span focus on time scope (scope means it needs explicitly specify its end) which usually used for tracing.
/// message focus on data context which usually used for logging/metric (since metric can be express by gauge which inclued by message).
/// one message can belong to a span, it means this log message happened in this span.
/// span itself contains one message, when logging, it just fill message with some specific info (startTime, endTime, duration, id ...)，
/// then every target can transform this message into their own backend's implementation (like zipkin,jaeger,opentracing...).
/// since span can be finished, so they will have a logger and send it's message at that time.
type Span =
  inherit IDisposable

  /// log its data into registry
  abstract finish: unit -> Promise<unit>
  /// the id of this span
  abstract id: string
  /// create child span id, make the span id has some relationship
  abstract generateChildId: unit -> string
