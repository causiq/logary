namespace Logary

open System
open System.Diagnostics
open System.Globalization
open Logary.Metric
open Logary.Trace
open System.Threading.Tasks
open FSharp.Control.Tasks.Builders
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Routing

module RequestInfo =
  let getLabels (ctx: HttpContext, ilogger: Logger, conf: MetricConf) (route: RouteValueDictionary option): Map<string, string> =
    let mutable labels = Map.empty

    let getRouteValue key =
      route
        |> Option.map (fun route ->
          match route.TryGetValue key with
          | true, (:? string as value) -> value
          | _ -> ""
        )
        |> Option.defaultValue ""

    let add key value =
      if not (String.IsNullOrWhiteSpace key) && not (String.IsNullOrWhiteSpace value) then
        labels <- labels |> Map.add key value

    for label in conf.labelNames do
      match label with
      | "action"
      | "controller" ->
        getRouteValue label
          |> add label
      | "code" ->
        ctx.Response.StatusCode.ToString(CultureInfo.InvariantCulture)
          |> add label
      | "method" ->
        ctx.Request.Method
          |> add label
      | unknown ->
        let message = sprintf "Unknown label value '%s' configured in metric conf for metric '%s'." unknown conf.name
        ilogger.debug(message)

    labels

[<AllowNullLiteral>]
type ICapturedRouteDataFeature =
  abstract values: RouteValueDictionary

[<Sealed>]
type CapturedRouteDataFeature() =
  let routeValues = RouteValueDictionary()
  interface ICapturedRouteDataFeature with
    member val values = routeValues with get

module internal RouteData =
  let RouteSpecific = Set [ "action"; "controller" ]

[<Sealed>]
type SpanMiddleware(next: RequestDelegate, logary: LogManager, logger: Logger) =
  let fake = MetricConf.create("http_span_fake_metric", "Fake metric conf", U.Scalar)

  member __.Invoke(ctx: HttpContext): Task =
    task {
      let route =
        let route = ctx.Features.Get<ICapturedRouteDataFeature>()
        if not (isNull route) then route.values
        else ctx.GetRouteData().Values

      let labels = RequestInfo.getLabels (ctx, logary.runtimeInfo.logger, fake) (Some route)

      let spanLogger, ctx = logger.startSpan ctx
      spanLogger.enableStreaming()

      try
        do! next.Invoke(ctx)
        spanLogger.finish(ctx, fun m -> m.setFieldValues labels)
          |> ignore
        return ctx
      with e ->
        spanLogger.finishWithExn(ctx, e)
          |> ignore
        return e.reraise()
    } :> _

/// Allows subsequent middlewares to access this middleware's cached route data values.
/// This isolates downstream middlewares from runtime changes to route data.
/// Should run after `UseRouting()`.
/// Data is stored in context via `ICapturedRouteDataFeature`.
type CaptureRouteDataMiddleware(next: RequestDelegate) =
  member __.Invoke(ctx: HttpContext): Task =
    let actual = ctx.GetRouteData()

    if isNull actual || actual.Values.Count <= 0 then next.Invoke ctx else

    let captured = CapturedRouteDataFeature()

    for KeyValue (key, value) in actual.Values do
      (captured :> ICapturedRouteDataFeature).values.Add(key, value)

    ctx.Features.Set<ICapturedRouteDataFeature>(captured)

    next.Invoke ctx


[<AbstractClass>]
type MetricMiddlewareBase<'a when 'a :> IMetric> (logary: LogManager, builder: MetricBuilder<'a>) =
  let hasRouteSpecific =
    Set builder.conf.labelNames |> Set.intersect RouteData.RouteSpecific |> Set.isEmpty |> not

  let baseMetric = logary.metrics.getOrCreate builder

  member private x.resolve (ctx: HttpContext, ?route: RouteValueDictionary) =
    let labels = RequestInfo.getLabels (ctx, logary.runtimeInfo.logger, builder.conf) route
    baseMetric.withLabels labels

  member x.getMetric (ctx: HttpContext): 'a =
    if not hasRouteSpecific then x.resolve(ctx) else
    let route = ctx.Features.Get<ICapturedRouteDataFeature>()
    if not (isNull route) then x.resolve(ctx, route.values)
    else x.resolve(ctx, ctx.GetRouteData().Values)

[<Sealed>]
type DurationHistogramMiddleware(next: RequestDelegate, logary: LogManager) =
  inherit MetricMiddlewareBase<IHistogram>(logary, Conventions.http_server_request_duration_seconds)
  member __.Invoke(ctx): Task =
    let metric = base.getMetric(ctx)
    task {
      let started = Stopwatch.getTimestamp()
      try
        do! next.Invoke(ctx)
      finally
        let dur = Stopwatch.toDuration (Stopwatch.getTimestamp() - started)
        metric.observe(dur.TotalSeconds)
    } :> _

[<Sealed>]
type RequestCountMiddleware(next: RequestDelegate, logary: LogManager) =
  inherit MetricMiddlewareBase<IGauge>(logary, Conventions.http_server_request_count)

  member __.Invoke(ctx): Task =
    let metric = base.getMetric ctx
    task {
      try
        do! next.Invoke ctx
      finally
        metric.inc 1.
    } :> _