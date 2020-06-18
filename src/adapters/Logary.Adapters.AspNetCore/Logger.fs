namespace Microsoft.Extensions.Logging

open Logary
open Microsoft.AspNetCore.Http

type LogaryLogger(inner: Logger) =
  interface ILogger with
    member x.BeginScope<'TState>(_: 'TState) =
      inner.scoped inner.name :> _

    member x.IsEnabled logLevel =
      inner.level <= logLevel.asLogary

    member x.Log<'TState> (logLevel, (* event id *) _, state: 'TState, ex, formatter) =
      // don't log more verbose log messages than we have to
      if inner.level > logLevel.asLogary then () else
      // closure-capture variables and send to Logary
      let message =
        let text = formatter.Invoke(state, ex)
        Model.EventMessage(text, level=logLevel.asLogary)
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
