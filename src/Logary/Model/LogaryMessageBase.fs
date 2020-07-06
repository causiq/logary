namespace Logary.Model

open System.Collections.Generic
open Logary
open Logary.Internals
open Logary.Trace

/// When values are passed into this base class, they are owned by it and must not be mutated from the outside.
[<AbstractClass>]
type LogaryMessageBase(kind, ?timestamp, ?messageId, ?name, ?level,
                       ?ctx: IReadOnlyDictionary<_,_>, ?fs: IReadOnlyDictionary<_,_>,
                       ?gauges: IReadOnlyDictionary<_,_>,
                       ?received, ?parentSpanId) =
  let ctx = lazy (ctx |> Option.map Dictionary<_,_> |> Option.defaultWith (fun () -> Dictionary<string, Value>()))
  let fs = lazy (fs |> Option.map Dictionary<_,_> |> Option.defaultWith (fun () -> Dictionary<string, Value>()))
  let gs = lazy (gauges |> Option.map Dictionary<_,_> |> Option.defaultWith (fun () -> Dictionary<string, Gauge>()))
  let messageId = messageId |> Option.defaultWith Id.create
  let ts = timestamp |> Option.defaultWith Global.getTimestamp

  abstract kind: MessageKind with get, set
  abstract id: Id with get, set
  abstract name: PointName with get, set
  abstract level: LogLevel with get, set
  abstract received: EpochNanoSeconds option with get, set
  abstract timestamp: EpochNanoSeconds with get, set
  abstract context: Dictionary<string, Value> with get
  abstract fields: Dictionary<string, Value> with get
  abstract gauges: Dictionary<string, Gauge> with get
  abstract parentSpanId: SpanId option with get, set

  default val kind = kind with get, set
  default val id = messageId with get, set
  default val name = defaultArg name PointName.empty with get, set
  default val level = defaultArg level Debug with get, set
  default val received = received with get, set
  default val timestamp = ts with get, set
  default val context = ctx.Value
  default val fields = fs.Value
  default val gauges = gs.Value
  default val parentSpanId = parentSpanId with get, set

  /// You can set this property to a `Metric.BasicConf` counter value, to auto-export the gauge message as a counter.
  /// Ensure that you set all the BasicConf's labels in the logged message fields/context.
  ///
  /// TO CONSIDER: making this field internal, hiding behind methods instead
  member val counterConf: Metric.MetricConf option = None with get, set

  /// Where to route this message
  member val internal targets: Set<string> = Set.empty with get, set

  /// Whether to wait for all targets' buffers to accept this message before committing to the caller alternative.
  member val internal waitForTargets = false with get, set

  member x.ensureName altName =
    if x.name.isEmpty then x.name <- altName

  member x.setCallerInfo(?site: string, ?file: string, ?lineNo: int, ?colNo: int) =
    site |> Option.iter (fun site -> x.fields.["site"] <- Value.Str site)
    file |> Option.iter (fun file -> x.fields.["file"] <- Value.Str file)
    colNo |> Option.iter (fun colNo -> x.fields.["colNo"] <- Value.Int64 (int64 colNo))
    lineNo |> Option.iter (fun lineNo -> x.fields.["lineNo"] <- Value.Int64 (int64 lineNo))

  member x.setCallerInfo(info: StackFrame) =
    x.setCallerInfo(?site=info.site, ?file=info.file, ?lineNo=info.lineNo, ?colNo=info.colNo)

  /// Adds an exception to the DTO.
  abstract addExn: e: exn * ?level: LogLevel -> unit

  default x.addExn(e: exn, ?level) =
    level |> Option.iter (fun level -> x.level <- level)
    x.setField("exn.message", Value.Str e.Message)
    x.setField("exn.helpLink", Value.Str e.HelpLink)
    x.setField("exn.source", Value.Str e.Source)
    x.setField("exn.string", Value.Str (e.ToString()))

  abstract cloneAndUpdate: builder: (LogaryMessageBase -> unit) -> LogaryMessageBase

  member x.setContext(key: string, value: Value) =
    x.context.[key] <- value

  member x.setContextValues(other: IReadOnlyDictionary<string, Value>) =
    for (KeyValue (k, v)) in other do x.context.[k] <- v
  member x.setContextValues(other: IReadOnlyDictionary<string, string>) =
    for (KeyValue (k, v)) in other do x.context.[k] <- Value.Str v

  member x.setFieldValues(other: IReadOnlyDictionary<string, Value>) =
    for (KeyValue (k, v)) in other do x.fields.[k] <- v

  member x.setFieldValues(other: IReadOnlyDictionary<string, string>) =
    for (KeyValue (k, v)) in other do x.fields.[k] <- Value.Str v

  member x.setGaugeValues(other: IReadOnlyDictionary<string, Gauge>) =
    for (KeyValue (k, v)) in other do x.gauges.[k] <- v

  member x.setGauge(key, gauge) =
    x.gauges.[key] <- gauge

  member x.setField(key: string, value: Value) =
    x.fields.[key] <- value

  interface Logary.LogaryMessage with
    member x.kind = x.kind
    member x.id = x.id
    member x.name = x.name
    member x.level = x.level
    member x.received = x.received
    member x.timestamp = x.timestamp
    member x.context = ctx.Value :> _
    member x.fields = fs.Value :> _
    member x.gauges = gs.Value :> _

    member x.parentSpanId = x.parentSpanId
