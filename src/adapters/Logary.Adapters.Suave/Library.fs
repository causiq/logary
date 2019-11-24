namespace Suave

open System
open Suave.Logging
open Logary
open Logary.Message
open Logary.Trace
open Logary.Trace.Propagation
open Logary.Adapters.Facade

[<AutoOpen>]
module internal SystemEx =
  open System
  open System.Runtime.ExceptionServices

  type Exception with
    member x.reraise () =
      ExceptionDispatchInfo.Capture(x).Throw()
      Unchecked.defaultof<_>

module LogaryAdapter =
  let getter: Getter<HttpContext> =
    fun ctx nameOrPrefix ->
      ctx.request.headers
        |> List.filter (fun (name, _) -> name.StartsWith(nameOrPrefix, StringComparison.InvariantCultureIgnoreCase))
        |> List.map (fun (name, value) -> name, value :: [])

  let setter: Setter<HttpContext> =
    fun (k: string, vs) ctx ->
      if List.isEmpty vs then ctx else
      let combinedHeaderValue = System.String.Join(",", vs)
      let rec replaceFirstItem = function
        | [], acc ->
          // place at back
          List.rev ((k, combinedHeaderValue) :: acc)
        | (kC: string, _) :: tail, acc when kC.ToLowerInvariant() = k ->
          // replace and return
          List.rev ((kC, combinedHeaderValue) :: acc) @ tail
        | _ :: tail, acc ->
          // ignore, already replaced
          replaceFirstItem (tail, acc)
      { ctx with response = { ctx.response with headers = replaceFirstItem (ctx.response.headers, []) } }

  let UserStateLoggerKey = "Logary.Trace.SpanLogger"

[<AutoOpen>]
module SuaveEx =
  open LogaryAdapter

  type Logger with
    member x.startSpan (ctx: HttpContext, ?propagator) =
      let propagator = defaultArg propagator Jaeger.propagator
      let started = Global.timestamp()
      let attrs, parentO = propagator.extract(getter, ctx)
      // https://github.com/open-telemetry/opentelemetry-specification/issues/210
      // https://github.com/open-telemetry/opentelemetry-python/pull/89/files
      let span =
        x.buildSpan(sprintf "%O %s" ctx.request.method ctx.request.url.AbsolutePath, ?parent=parentO)
         .setStarted(started)
         .setKind(SpanKind.Server)
         .setAttribute("component", "http")
         .setAttribute("http.method", ctx.request.method.ToString())
         .setAttribute("http.route", ctx.request.url.AbsolutePath)
         .setAttribute("http.query", ctx.request.url.Query)
         .setAttributes(attrs)
         .start()

      if ctx.request.queryFlag "debug" then span.debug()

      let nextCtx = { ctx with userState = ctx.userState |> Map.add UserStateLoggerKey (box span) }

      span, nextCtx

  type HttpContext with
    member x.logger: SpanLogger =
      match x.userState |> Map.tryFind UserStateLoggerKey with
      | Some value ->
        unbox value
      | None ->
        failwithf "Logary.Adapters.Suave Failed to find key '%s' in HttpContext.userState. Did you wrap your app using `withTracing`?" UserStateLoggerKey

[<AutoOpen>]
module SpanLoggerEx =
  type SpanLogger with
    member x.finish (res: HttpContext option) =
      res |> Option.iter (fun ctx ->
        if ctx.response.status.code >= 500 then
          x.setStatus(SpanCanonicalCode.InternalError)
          x.setAttribute("error", true)
        elif ctx.response.status.code >= 400 then x.setAttribute("http.bad_request", true)
        x.setAttribute("http.status_code", ctx.response.status.code)
        x.setAttribute("http.status_text", ctx.response.status.reason)
      )
      x.finish ()

    member x.finishWithExn (_: HttpContext, e: exn) =
      x.setAttribute("http.status_code", 500)
      x.setStatus(SpanCanonicalCode.InternalError, e.ToString())
      x.setAttribute("error", true)
      x.error (eventX "Unhandled error" >> addExn e)
      x.finish()

module Filters =

  let spanLogger (f: SpanLogger -> WebPart): WebPart =
    context (fun ctx -> f ctx.logger)

  let span (f: Span -> WebPart): WebPart =
    context (fun ctx -> f ctx.logger)

  let withTracing (logger: Logger) (app: WebPart) =
    fun ctx ->
      async {
        let spanLogger, ctx = logger.startSpan ctx
        spanLogger.logThrough()
        try
          let! res = app ctx
          spanLogger.finish res
          return res
        with e ->
          spanLogger.finishWithExn (ctx, e)
          return e.reraise()
      }

