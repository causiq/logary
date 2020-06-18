namespace Logary.Ingestion.HTTP.CORS

open Microsoft.AspNetCore.Http
open NodaTime
open Giraffe

type HTTPOrigin = string

type OriginResult =
  /// Allow all origins
  | Star
  /// Allow the origin passed by the Origin request header
  | Origin of origin:string
  /// Don't return this header
  | Reject

  static member allowAll _ =
    Star

  static member allowRequester origin =
    Origin origin

  member x.asHttpHandler: HttpHandler =
    match x with
    | Star -> setHttpHeader "Access-Control-Allow-Origin" "*"
    | Origin origin -> setHttpHeader "Access-Control-Allow-Origin" origin
    | Reject -> fun _ _ -> skipPipeline // bails out of the pipeline

type CORSConfig =
  { /// Toggle CORS-support on/off. This is toggled on if you supply a `accessControlAllowOrigin` callback
    allowCORS: bool
    /// https://developer.mozilla.org/en-US/docs/Web/HTTP/CORS#The_HTTP_response_headers
    accessControlAllowOrigin: HTTPOrigin -> OriginResult
    /// Access-Control-Request-Method -> your allowed methods
    accessControlAllowMethods: string -> string list
    /// Access-Control-Request-Headers -> your allowed Origin
    accessControlAllowHeaders: string list -> string list
    /// For how long does this policy apply?
    accessControlMaxAge: Duration
  }
  static member create(?accessControlAllowOrigin, ?accessControlAllowHeaders, ?accessControlMaxAge) =
    let allowCORS, acao, acah =
      Option.isSome accessControlAllowOrigin,
      accessControlAllowOrigin |> Option.defaultValue (fun _ -> Star),
      accessControlAllowHeaders |> Option.defaultValue id
    { allowCORS = allowCORS
      accessControlAllowOrigin = acao
      accessControlAllowHeaders = acah
      accessControlAllowMethods = fun _ -> HttpMethods.Post :: []
      accessControlMaxAge = defaultArg accessControlMaxAge (Duration.FromDays 1) }


module API =
  open System

  let withOrigin config next =
    warbler (fun (_, ctx) ->
      match ctx.GetRequestHeader "origin" with
      | Ok origin ->
        next (config.accessControlAllowOrigin origin)
      | Result.Error _ ->
        next (config.accessControlAllowOrigin "http://localhost"))

  let CORS (config: CORSConfig) =
    if config.allowCORS then
      warbler (fun (_, ctx) ->
        let h =
          ctx.GetRequestHeader "access-control-request-headers"
          |> Result.map (fun h  -> h.Split([|','|], StringSplitOptions.RemoveEmptyEntries) |> List.ofArray)
          |> function Result.Error _ -> "Content-Type" :: [] | Ok x -> x
        let ah =
          config.accessControlAllowHeaders h
          |> String.concat ", "
        let m =
          match ctx.GetRequestHeader "access-control-request-method" with
          | Result.Error _ -> HttpMethods.Post
          | Result.Ok m -> m

        let am =
          config.accessControlAllowMethods m
          |> List.map (fun m -> m.ToString())
          |> String.concat ", "
        let aa = string config.accessControlMaxAge.TotalSeconds

        withOrigin config (fun ao -> ao.asHttpHandler)
        >=> setHttpHeader "Access-Control-Allow-Methods" am
        >=> setHttpHeader "Access-Control-Allow-Headers" ah
        >=> setHttpHeader "Access-Control-Max-Age" aa)
        >=> setHttpHeader "Content-Type" "text/plain; charset=utf-8"
        >=> text ""
    else
      fun _ _ -> skipPipeline


