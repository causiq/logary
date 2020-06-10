namespace Logary.Trace

open Logary
open Logary.Trace
open Logary.Trace.Propagation
open Microsoft.AspNetCore.Http
open System.Runtime.CompilerServices

[<Extension; AutoOpen>]
module LoggerEx =
  open Logary.Trace.Propagation.HttpContext

  let private startNewSpanFrom (x: Logger, ctx: HttpContext, propagator: Propagator option) =
    let propagator = propagator |> Option.defaultWith (fun () -> Combined.choose [ Jaeger.propagator; W3C.propagator ])
    let attrs, parentO = propagator.extract(getter, ctx)
    // https://github.com/open-telemetry/opentelemetry-specification/issues/210
    // https://github.com/open-telemetry/opentelemetry-python/pull/89/files
    let span =
      x.buildSpan(sprintf "%O %s" ctx.Request.Method ctx.Request.Path.Value, ?parent=parentO)
       .setKind(SpanKind.Server)
       .setAttribute("component", "http")
       .setAttribute("http.method", ctx.Request.Method)
       .setAttribute("http.route", ctx.Request.Path.Value)
       .setAttribute("http.query", ctx.Request.QueryString.Value)
       .setAttributes(attrs)
       .startWith(x)

    if ctx.Request.Query.ContainsKey "debug" then span.debug()

    ctx.Items.[UserStateLoggerKey] <- span

    span, ctx

  [<Extension; CompiledName "StartSpan">]
  let startSpan (x: Logger, ctx: HttpContext, propagator: Propagator option) =
    if isNull ctx then nullArg "ctx"
    match ctx.Items.TryGetValue UserStateLoggerKey with
    | false, _ ->
      startNewSpanFrom (x, ctx, propagator)
    | true, existing ->
      eprintfn "'startSpan' called more than once for a HttpContext"
      existing :?> SpanLogger, ctx

  type Logger with
    member x.startSpan (ctx: HttpContext, ?propagator: Propagator) =
      startSpan (x, ctx, propagator)


[<Extension; AutoOpen>]
module SpanLoggerEx =

  [<Extension; CompiledName "Finish">]
  let finish (x: SpanLogger, ctx: HttpContext) =
    if ctx.Response.StatusCode >= 500 then
      x.setStatus(SpanCanonicalCode.InternalError)
      x.setAttribute("error", true)
    elif
      ctx.Response.StatusCode >= 400 then x.setAttribute("http.bad_request", true)
    x.setAttribute("http.status_code", ctx.Response.StatusCode)
    x.finish ()

  [<Extension; CompiledName "FinishWithException">]
  let finishWithExn (x: SpanLogger, _: HttpContext, ex: exn) =
    x.setAttribute("http.status_code", 500)
    x.setStatus(SpanCanonicalCode.InternalError, ex.ToString())
    x.setAttribute("error", true)
    x.error("Unhandled exception", fun m -> m.addExn ex)
    x.finish()

  type SpanLogger with
    member x.finish (ctx: HttpContext) = finish (x, ctx)
    member x.finishWithExn (ctx: HttpContext, ex: exn) = finishWithExn (x, ctx, ex)



namespace Microsoft.Extensions.Logging

open FSharp.Control.Tasks.V2
open Logary
open Logary.Trace
open Logary.Trace.Propagation
open Logary.Adapters.AspNetCore
open Microsoft.AspNetCore.Http
open System.Runtime.CompilerServices

[<Extension; AutoOpen>]
module HttpContextEx =

  [<Extension; CompiledName "SpanLogger">]
  let spanLogger (httpCtx: HttpContext) =
    if isNull httpCtx then None else
    match httpCtx.Items.TryGetValue HttpContext.UserStateLoggerKey with
    | false, _ ->
      None
    | true, value ->
      Some (value :?> SpanLogger)

  [<Extension; CompiledName "SpanLogger">]
  let logger (httpCtx: HttpContext) =
    match spanLogger httpCtx with
    | None ->
      Log.create "Logary.Adapters.AspNetCore"
    | Some logger ->
      logger :> _

  [<Extension; CompiledName "GetOrCreateSpanLogger">]
  let getOrCreateSpanLogger (httpCtx: HttpContext, name: string, propagator: Propagator option) =
    match spanLogger httpCtx with
    | None when not (isNull httpCtx) ->
      let created = Log.create name
      let logger, _ = created.startSpan(httpCtx, ?propagator=propagator)
      logger

    | None ->
      let created = Log.create name
      created.buildSpan(name).startWith created

    | Some value ->
      value

  type HttpContext with
    member x.logger = logger x
    member x.spanLogger = spanLogger x
    member x.getOrCreateSpanLogger(name: string, ?propagator) = getOrCreateSpanLogger (x, name, propagator)


type LogaryMiddleware(next: RequestDelegate, logger: Logger) =
  member __.Invoke (ctx: HttpContext) =
    let spanLogger, ctx = logger.startSpan ctx
    spanLogger.logThrough()
    task {
      try
        do! next.Invoke(ctx)
        spanLogger.finish ctx
          |> ignore
        return ctx
      with e ->
        spanLogger.finishWithExn (ctx, e)
          |> ignore
        return e.reraise()
    }