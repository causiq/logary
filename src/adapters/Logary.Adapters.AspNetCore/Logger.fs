namespace Microsoft.Extensions.Logging

open System
open System.Collections.Generic
open Logary
open Logary.Internals.TypeShape.Core.Core
open Microsoft.AspNetCore.Http

module MicrosoftQuirks =
  /// Because of course they're doing it their own way, like always. In the "RequestId" case, they are
  /// actually sending in a real SpanContext, but with their own non-standard SpanId and TraceId schemas.
  let getAttributes (state: 'tstate): seq<KeyValuePair<string, Value>> =
    match shapeof<'tstate> with
    | Shape.Enumerable e ->
      match e.Element with
      | Shape.KeyValuePair kvp ->
        match kvp.Key, kvp.Value with
        | Shape.String, _ ->
          e.Accept { new IEnumerableVisitor<seq<KeyValuePair<string, Value>>> with
            member __.Visit<'E, 'T when 'E :> seq<'T>>() =
              box state :?> seq<'T>
                |> Seq.map (fun t -> box t :?> KeyValuePair<string, obj>)
                |> Seq.map (fun kv -> KeyValuePair<_,_>(kv.Key, Value.Str (kv.Value.ToString())))
          }
        | _ -> Seq.empty
      | _ -> Seq.empty
    | _ -> Seq.empty

type LogaryLogger(inner: Logger) =
  interface ILogger with
    member x.BeginScope<'TState>(state: 'TState) =
      let spanLogger = inner.scoped inner.name
      for KeyValue (k, attr) in MicrosoftQuirks.getAttributes state do
        spanLogger.setAttribute(k, attr)
      spanLogger :> IDisposable

    member x.IsEnabled logLevel =
      inner.level <= logLevel.asLogary

    member x.Log<'TState> (logLevel, (* event id *) _, state: 'TState, ex, formatter) =
      // don't log more verbose log messages than we have to
      if inner.level > logLevel.asLogary then () else
      // closure-capture variables and send to Logary
      let message =
        let text = formatter.Invoke(state, ex)
        Model.Event(text, level=logLevel.asLogary)
      inner.log message


type LogaryLoggerFactory(accessor: IHttpContextAccessor, logary: LogManager) =
  interface ILoggerFactory with
    member x.AddProvider _ =
      failwithf "You should not use the ASP.Net providers when logging with Logary. Logary has its own \"providers\" called \"targets\". Have a look at https://github.com/logary/logary and https://logary.tech for documentation or file an issue if your target is missing"

    member x.CreateLogger name =
      match accessor.HttpContext.spanLogger with
      | None ->
        let inner = logary.getLogger name
        LogaryLogger(inner) :> ILogger
      | Some logger -> // here we have a nice span logger! 😋
        LogaryLogger(logger) :> ILogger

    member x.Dispose () = ()
