namespace Microsoft.Extensions.Logging

open FSharp.Control.Tasks.V2
open System
open Logary
open Logary.Message
open Logary.Trace
open Logary.Trace.Propagation
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Hosting
open System.Runtime.CompilerServices

[<AutoOpen>]
module internal SystemEx =
  open System.Runtime.ExceptionServices

  type Exception with
    member x.reraise () =
      ExceptionDispatchInfo.Capture(x).Throw()
      Unchecked.defaultof<_>


module LogaryAdapter =

  let getter: Getter<HttpContext> =
    fun ctx nameOrPrefix ->
      [ for h in ctx.Request.Headers do
          if h.Key.StartsWith(nameOrPrefix, StringComparison.InvariantCultureIgnoreCase) then
            yield h.Key, List.ofArray (h.Value.ToArray())
      ]

  let setter: Setter<HttpContext> =
    fun (k: string, vs) ctx ->
      ctx.Response.Headers.SetCommaSeparatedValues(k, Array.ofList vs)
      ctx

  let UserStateLoggerKey = "Logary.Trace.SpanLogger"

[<Extension; AutoOpen>]
module LoggerEx =
  open LogaryAdapter

  [<Extension; CompiledName "StartSpan">]
  let startSpan (x: Logger, ctx: HttpContext, propagator: Propagator option) =
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
       .start()

    if ctx.Request.Query.ContainsKey "debug" then span.debug()

    ctx.Items.[UserStateLoggerKey] <- span

    span, ctx

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
  let finishWithExn (x: SpanLogger, ctx: HttpContext, ex: exn) =
    x.setAttribute("http.status_code", 500)
    x.setStatus(SpanCanonicalCode.InternalError, ex.ToString())
    x.setAttribute("error", true)
    x.error (eventX "Unhandled error" >> addExn ex)
    x.finish()

  type SpanLogger with
    member x.finish (ctx: HttpContext) = finish (x, ctx)
    member x.finishWithExn (ctx: HttpContext, ex: exn) = finishWithExn (x, ctx, ex)

type LogaryMiddleware(next: RequestDelegate, logger: Logger) =
  member __.Invoke (ctx: HttpContext) =
    let spanLogger, ctx = logger.startSpan ctx
    spanLogger.logThrough()
    task {
      try
        do! next.Invoke(ctx)
        spanLogger.finish ctx
        return ctx
      with e ->
        spanLogger.finishWithExn (ctx, e)
        return e.reraise()
    }

type LogaryStartupFilter() =
  interface IStartupFilter with
    member x.Configure(next: Action<IApplicationBuilder>) =
      let build (next: Action<IApplicationBuilder>) (b: IApplicationBuilder) =
        b.UseMiddleware<LogaryMiddleware>() |> ignore
        next.Invoke b
      Action<_>(build next)

module internal Impl =

  let addServices (logary: LogManager) _ (s: IServiceCollection) =
    s.AddSingleton(logary)
     .AddSingleton(logary.getLogger(logary.runtimeInfo.service))
     .AddTransient<IStartupFilter, LogaryStartupFilter>()
    |> ignore

[<Extension; AutoOpen>]
module IHostBuilderEx =
  open Impl

  [<Extension; CompiledName "UseLogary">]
  let useLogary (builder: IHostBuilder, logary: LogManager) =
     let cb = Action<_,_>(addServices logary)
     builder.ConfigureServices(cb)

  type IHostBuilder with
    member x.UseLogary(logary: LogManager) = useLogary (x, logary)

// OR, you can use:
[<Extension; AutoOpen>]
module IWebHostBuilderEx =
  open Impl

  [<Extension; CompiledName "UseLogary">]
  let useLogary (builder: IWebHostBuilder, logary: LogManager) =
     let cb = Action<_>(addServices logary ())
     builder.ConfigureServices(cb)

  type IWebHostBuilder with
    member x.UseLogary(logary: LogManager) = useLogary (x, logary)