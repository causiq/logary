module Program

open Hopac
open Suave
open Suave.Successful
open Suave.Operators
open Suave.Filters
open Suave.RequestErrors
open Suave.Logging
open Logary
open Logary.Message
open Logary.Trace
open Logary.Trace.Propagation
open Logary.Adapters.Facade
open Logary.Configuration
open Logary.Metric
open Logary.Targets

[<AutoOpen>]
module MoveMe =
  open System
  open System.Runtime.ExceptionServices

  type Exception with
    member x.reraise () =
      ExceptionDispatchInfo.Capture(x).Throw()
      Unchecked.defaultof<_>

module SuaveAdapter =

  let getter: Getter<HttpContext> =
    fun ctx nameOrPrefix ->
      ctx.request.headers
        |> List.filter (fun (name, _) -> name.StartsWith nameOrPrefix)
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

  type Logger with
    member x.startSpan (ctx: HttpContext) =
      let started = Global.timestamp()
      let attrs, _, parentO = Jaeger.extract getter ctx
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

  type HttpContext with
    member x.logger: SpanLogger =
      match x.userState |> Map.tryFind UserStateLoggerKey with
      | Some value ->
        unbox value
      | None ->
        failwithf "Logary.Adapters.Suave Failed to find key '%s' in HttpContext.userState. Did you wrap your app using `withTracing`?" UserStateLoggerKey

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

open Logary.Prometheus.Exporter
open SuaveAdapter

[<EntryPoint>]
let main _ =
  let logary =
    Config.create "Logary.Examples.Suave" "localhost"
    |> Config.ilogger (ILogger.LiterateConsole Verbose)
    |> Config.targets [
      LiterateConsole.create LiterateConsole.empty "console"
      Jaeger.create { Jaeger.empty with jaegerPort = 30831us } "jaeger"
    ]
    |> Config.build
    |> run

  LogaryFacadeAdapter.initialise<Suave.Logging.Logger> logary

  let logger = Logary.Log.create "Logary.Examples.Suave"

  let app: WebPart =
    choose [
      GET >=> path "/hello" >=> spanLogger (fun logger ->
        logger.info (eventX "Returning from the first route: 'GET /'")
        OK (sprintf "Hello World! My route has been going for %O so far..." logger.elapsed))

      POST >=> path "/" >=> OK "{\"hello\": \"world\"}"

      request (fun req -> NOT_FOUND (sprintf "What are you trying to do? %s wasn't found, anyway! :-s" req.path))
    ]

  use metrics = Exporter.run (ExporterConf.create(logary.metricRegistry))
  startWebServer defaultConfig (withTracing logger app)

  0