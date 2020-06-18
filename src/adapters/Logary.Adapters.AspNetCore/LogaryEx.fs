namespace Logary

open Logary
open Logary.Trace
open Logary.Trace.Propagation
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

  [<Extension; CompiledName "GetCreateLogger">]
  let getCreateLogger (httpCtx: HttpContext, name: string, propagator: Propagator option) =
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
    member x.getCreateLogger(name: string, ?propagator) = getCreateLogger (x, name, propagator)

