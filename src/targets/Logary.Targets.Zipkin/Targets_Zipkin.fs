/// A target for Zipkin.
module Logary.Targets.Zipkin

// https://github.com/racker/restkin
// https://github.com/racker/tryfer

open System
open System.IO
open System.Net
open System.Runtime.CompilerServices
open Hopac
open Hopac.Infixes
open Logary
open Logary.Internals
open Logary.Target
open Logary.Configuration
open Logary.Zipkin
open Microsoft.FSharp.Reflection

module internal Option =
  let orDefault a = Option.fold (fun s t -> t) a

open Option

/// Configuration for Zipkin target.
type ZipkinConf =
  { baseUri          : Uri
    collectorFactory : Uri -> ISpanCollector }

  /// Create a new configuration with a base uri and an optional
  /// rest client factory.
  /// Default port: 9411
  /// Default host: localhost
  static member create(?baseUri, ?fac, ?traceReq, ?formatter) =
    { baseUri = defaultArg baseUri (Uri "http://localhost:9411")
      collectorFactory = defaultArg fac (fun uri -> upcast HttpCollector uri) }

module internal Impl =

  type ZipkinState = 
    { activeSpans : Map<uint64, Span>
      collector   : ISpanCollector }

    interface IDisposable with
      member x.Dispose () =
        match x.collector with
        | :? IDisposable as d ->
          d.Dispose()

        | _ -> ()

  let toUInt64 = function
    | Int64 x ->
      uint64 x

    | other ->
      failwithf "Unexpected non-int64 value whilst trying to make a span out of Message.context: %A" other

  let toInt64 = function
    | Int64 x ->
      int64 x

    | other ->
      failwithf "Unexpected non-int64 value whilst trying to make a span out of Message.context: %A" other

  let makeSpan (message: Message) : Span = 
    let traceId, spanId =
      message |> Message.tryGetContext "traceId" |> Option.orDefault 0L |> uint64,
      message |> Message.tryGetContext "spanId" |> Option.orDefault 0L |> uint64

    let parentId =
      message |> Message.tryGetContext "parentId"

    let host = message |> Message.tryGetContext "host" |> Option.orDefault 0L
    let port = message |> Message.tryGetContext "port" |> Option.orDefault 0L

    let debug = false
    let trace = TraceHeader(traceId, spanId, Option.toNullable parentId)
    Span(trace, IPEndPoint(host, int port), PointName.format message.name)

  let val2str = Logary.Formatting.Json.format


  let val2bin (key:string, value: obj) =
    match value with
    | :? bool as b ->
      Annotations.Binary(key, b)

    | :? array<byte> as bin ->
      BinaryAnnotation(key, bin, AnnotationType.Bytes)

    | other ->
      Annotations.Binary(key, val2str other)

  let annotateSpan state (message : Message) = 
    match message |> Message.tryGetContext "spanId" with
    | Some (spanId : uint64) ->
      let span =
        match Map.tryFind spanId state.activeSpans with
        | None ->
          // TODO: handle client open client close
          // TODO: handle server open server close
          makeSpan message

        | Some s ->
          s

      let msgValue = Annotation(message.value,DateTime message.timestampTicks)
      span.Record msgValue

      message
      |> Message.getAllGauges
      |> Seq.map (fun (gaugeType, Gauge (v,u)) ->
         let vs = v.ToString()
         let us = Units.symbol u
         Annotation(gaugeType + ": " + vs + " " + us, DateTime message.timestampTicks))
      |> Seq.iter span.Record

      message
      |> Message.getAllFields
      |> Seq.map val2bin
      |> Seq.iter span.Record

      { state with activeSpans = Map.add spanId span state.activeSpans }

    | _ ->
      state

  let loop (config : ZipkinConf)
           (ri : RuntimeInfo, api : TargetAPI) =

    let rec loop state : Job<unit> =
      let collector = state.collector
      Alt.choose [
        api.shutdownCh ^=> fun ack ->
          job {
            do! Job.Scheduler.isolate <| fun _ ->
              try
                (state :> IDisposable).Dispose ()
              with e ->
                Message.eventError "Zipkin target disposing connection"
                |> Message.addExn e
                |> Logger.logSimple ri.logger

            do! ack *<= () :> Job<_>
          }

        RingBuffer.take api.requests ^=> function
          | Log(logMsg, ack) ->
            let nextState = annotateSpan state logMsg
            ack *<= () |> Job.bind (fun _ -> loop nextState)

          | Flush(ack, nack) -> 
            // TODO: move this to a flush mechanism instead
            let spans =
              state.activeSpans
              |> Map.toSeq
              |> Seq.map snd
              |> Seq.toArray

            job {
              do! Job.fromUnitTask (fun _ -> collector.CollectAsync spans)
              do! IVar.fill ack () 
              return! loop { state with activeSpans = Map.empty }
            }

      ] :> Job<_>

    loop { collector = config.collectorFactory config.baseUri
           activeSpans = Map.empty }

/// Create a new Zipkin target
let create conf = TargetConf.createSimple (Impl.loop conf)

/// Use with LogaryFactory.New( s => s.Target< HERE >() )
type Builder(conf, callParent : Target.ParentCallback<Builder>) = 
  member x.WithConfig(conf : ZipkinConf) =
    !(callParent <| Builder(conf, callParent))

  new(callParent : Target.ParentCallback<_>) =
    Builder(ZipkinConf.create (), callParent)

  interface Target.SpecificTargetConf with
    member x.Build name =
      create conf name