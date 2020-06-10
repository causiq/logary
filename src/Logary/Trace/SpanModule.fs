namespace Logary.Trace

open System.Runtime.CompilerServices
open Hopac
open Logary
open Logary.Trace.Propagation

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Span =
  /// Use the instance methods on the returned object
  let create label = SpanBuilder(label)
  let start (x: SpanBuilder) = x.start()
  let startWith (x: SpanBuilder) logger = x.startWith logger

[<AutoOpen>]
module LoggerEx =
  type Logger with
    /// Also see `startSpan`, `startChild`
    member x.buildSpan(label: string,
                       ?parent: SpanContext,
                       ?kind: SpanKind,
                       ?enhance: SpanBuilder -> SpanBuilder,
                       ?transform: Model.SpanMessage -> Model.SpanMessage,
                       ?enableAmbient: bool) =
      let builder =
        SpanBuilder(label).setKind(defaultArg kind SpanKind.Internal)
          |> Option.defaultValue id enhance

      parent |> Option.iter (builder.withParent >> ignore)

      builder
        .withTransform(Option.defaultValue id transform)
        .setAmbientEnabled(defaultArg enableAmbient false)

    member x.buildSpan (label, parent: SpanMessage, ?kind, ?enhance, ?transform, ?enableAmbient) =
      let nextKind = defaultArg kind parent.kind.next
      x.buildSpan (label, parent.context, nextKind, ?enhance=enhance, ?transform=transform, ?enableAmbient=enableAmbient)

    member x.startSpan (label, ?parent: SpanMessage, ?kind, ?enhance, ?transform, ?enableAmbient) =
      let parentO = parent |> Option.map (fun p -> p.context)
      let builder = x.buildSpan(label, ?kind=kind, ?parent=parentO, ?enhance=enhance, ?transform=transform, ?enableAmbient=enableAmbient)
      builder.startWith x


[<AutoOpen>]
module SpanLoggerEx =
  type SpanLogger with
    member x.startChild (label: string, ?kind, ?enhance: SpanBuilder -> SpanBuilder, ?transform: Model.SpanMessage -> Model.SpanMessage, ?enableAmbient: bool) =
      let kind = defaultArg kind x.kind.next
      let logger = x :> Logger
      let builder = logger.buildSpan(label, x.context, kind, ?enhance=enhance, ?transform=transform, ?enableAmbient=enableAmbient)
      builder.startWith x

    member x.inject (propagator: Propagator, setter: Setter<'t>, target: 't): 't =
      propagator.inject (setter, x.context, target)

    member x.injectWith (propagator: Propagator, setter: Setter<'t>): 't -> 't =
      fun t ->
        propagator.inject(setter, x.context, t)

    member x.timeFun (f: 'input -> 'res, ?label: string, // ?builder,
                      [<CallerMemberName>] ?memberName: string,
                      [<CallerFilePath>] ?file: string,
                      [<CallerLineNumber>] ?lineNo: int)
                      : 'input -> 'res =

      let label =
        label
          |> Option.bind nullIsNone
          |> Option.orElse memberName
          |> Option.defaultValue "methodCall"

      fun input ->
        use scope = x.startChild(label)
        let start = Internals.Global.getTimestamp()
        let res = f input
        let finish = Internals.Global.getTimestamp()
        scope.finish(fun m ->
          m.addCallerInfo(defaultArg memberName "timeFun", ?file=file, ?lineNo=lineNo)
          m.timestamp <- start
          m.finished <- Some finish) |> ignore
        res


[<AutoOpen>]
module SpanOpsEx =
  type SpanOps with
    member x.setAttribute(key: string, f: float) = x.setAttribute(key, Value.Float f)
    member x.setAttribute(key: string, i: int64) = x.setAttribute(key, Value.Int64 i)
    member x.setAttribute(key: string, i: int) = x.setAttribute(key, Value.Int64 (int64 i))
    member x.setAttribute(key: string, i: int16) = x.setAttribute(key, Value.Int64 (int64 i))
    member x.setAttribute(key: string, i: bigint) = x.setAttribute(key, Value.BigInt i)
    member x.setAttribute(key: string, boolean: bool) = x.setAttribute(key, Value.Bool boolean)
    member x.setAttribute(key: string, string: string) = x.setAttribute(key, Value.Str string)

[<AutoOpen>]
module SpanOpsAdvancedEx =
  open Hopac.Infixes

  type SpanLogger with
    member x.finishAck (ts: EpochNanoSeconds) =
      x.finishWithAck(ts)
      >>=* function | Ok ack -> ack | _ -> Promise.unit

    member x.finishAck() =
      x.finishWithAck()
      >>=* function | Ok ack -> ack | _ -> Promise.unit
