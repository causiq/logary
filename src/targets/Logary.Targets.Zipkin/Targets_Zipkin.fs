namespace Logary.Targets

// https://github.com/racker/restkin
// https://github.com/racker/tryfer
/// A target for Zipkin.
module Zipkin = 
    open System
    open System.IO
    open System.Net
    open System.Runtime.CompilerServices
    open Hopac
    open Hopac.Infixes
    open Logary
    open Logary.Internals
    open Logary.Target
    open Logary.Formatting
    open Zipkin
    open Microsoft.FSharp.Reflection
        
    module internal Option = 
        let orDefault a = Option.fold (fun s t -> t) a
    
    open Option
    
    /// Configuration for Zipkin target.
    type ZipkinConf = 
        { baseUri : Uri
          collectorFactory : Uri -> ISpanCollector }
        
        /// Create a new configuration with a base uri and an optional
        /// rest client factory.
        /// Default port: 9411
        /// Default host: localhost
        static member Create(?baseUri, ?fac, ?traceReq, ?formatter) = 
            { baseUri = defaultArg baseUri (Uri "http://localhost:9411")
              collectorFactory = defaultArg fac (fun uri -> upcast HttpCollector uri) }
        
        static member Default = ZipkinConf.Create()
    
    type private ZipkinState = 
        { activeSpans : Map<uint64, Span>
          collector : ISpanCollector }
    
    let private makeSpan (message: Message): Span = 
        let (Int64 traceId) = message.context.["traceId"]
        let (Int64 spanId) = message.context.["spanId"]
        let parentId = Map.tryFind "parentId" message.context |> Option.map (fun (Int64 x) -> uint64(x))
        let (Int64 host) = message.context.["host"]
        let (Int64 port) = message.context.["port"]
        let debug = false
        let trace = TraceHeader(uint64(traceId), uint64(spanId), Option.toNullable parentId)
        Span(trace, IPEndPoint(host, int(port)), message.name.ToString())

    open Logary.Utils.Chiron.Mapping
    open Logary.Utils.Chiron.Formatting

    let private val2str = function
        | String s -> s
        | Float f -> f.ToString()
        | Int64 i64 -> i64.ToString()
        | BigInt big -> big.ToString()
        | Fraction (x, y) -> x.ToString() + "/" + y.ToString()
        | Object fields -> 
            let json = Json.serialize fields
            Json.format json
        | Array vals -> 
            let json = Json.serialize vals
            Json.format json
        | Bool b -> if b then "true" else "false"
        | Binary (bin, contentType) -> contentType + "=" + Convert.ToBase64String bin
        
    let private val2ann orient timestampTicks = function 
        | Gauge (v, u) -> Annotation(Units.formatWithUnit orient u v, DateTime timestampTicks)
        | Derived (v, u) -> Annotation(Units.formatWithUnit orient u v, DateTime timestampTicks)
        | Event t -> Annotation(t, DateTime timestampTicks)

    let private val2bin (name:PointName, (Field (value, maybeUnit))) =
        let key = name.ToString()
        match value with
        | Bool b -> Annotations.Binary(key, b)
        | Binary (bin, contentType) -> BinaryAnnotation(key, bin, AnnotationType.Bytes)
        | other -> Annotations.Binary(key, val2str other)

    let private annotateSpan state (message : Message) = 
        match Map.tryFind "spanId" message.context with
        | Some (Int64 recId) ->
            let spanId = (uint64 recId)
            let span = 
                match Map.tryFind spanId state.activeSpans with
                | None -> makeSpan message
                | Some s -> s
            let annotation = val2ann Units.UnitOrientation.Suffix message.timestampTicks message.value
            span.Record annotation
            message.fields
            |> Map.toSeq
            |> Seq.map val2bin 
            |> Seq.iter (span.Record)
            { state with activeSpans = Map.add spanId span state.activeSpans }
        | _ -> state 

    let loop (config : ZipkinConf) (ri : RuntimeInfo) (requests : RingBuffer<TargetMessage>) (shutdown : Ch<IVar<unit>>) = 
        let rec loop state : Job<unit> = 
            let collector = state.collector
            Alt.choose [ shutdown ^=> fun ack -> 
                             match collector with
                             | :? IDisposable as d -> d.Dispose()
                             | _ -> ()
                             (ack *<= () :> Job<_>)
                         RingBuffer.take requests ^=> function 
                         | Log(logMsg, ack) -> 
                             job { 
                                 let nextState = annotateSpan state logMsg
                                 do! ack *<= ()
                                 return! loop nextState
                             }
                         | Flush(ack, nack) -> 
                             job { 
                                 let spans =
                                    state.activeSpans
                                    |> Map.toSeq
                                    |> Seq.map snd
                                    |> Seq.toArray
                                 do! collector.CollectAsync spans |> Async.AwaitTask
                                 do! Ch.give ack () <|> nack
                                 return! loop { state with activeSpans = Map.empty }
                             } ] :> Job<_>
        loop { collector = config.collectorFactory (config.baseUri); activeSpans = Map.empty }
    
    /// Create a new Zipkin target
    let create conf = TargetUtils.stdNamedTarget (loop conf)
    
    /// C# interop
    [<CompiledName"Create">]
    let create' (conf, name) = create conf name
    
    /// Use with LogaryFactory.New( s => s.Target< HERE >() )
    type Builder(conf, callParent : FactoryApi.ParentCallback<Builder>) = 
        member x.WithConfig(conf : ZipkinConf) = !(callParent <| Builder(conf, callParent))
        new(callParent : FactoryApi.ParentCallback<_>) = Builder(ZipkinConf.Default, callParent)
        interface Logary.Target.FactoryApi.SpecificTargetConf with
            member x.Build name = create conf name
