namespace Logary

open Logary.Internals
open System.Net
open System.Diagnostics

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Middleware =

  /// This is the identity middleware.
  [<CompiledName "Identity">]
  let identity: Middleware =
    fun next msg -> next msg

  /// Sets the host name as a context value
  [<CompiledName "Host">]
  let host host: Middleware =
    fun next msg ->
      Message.setContext KnownLiterals.HostContextName host msg
      |> next

  /// Sets the host name as a context value
  [<CompiledName "DnsHost">]
  let dnsHost: Middleware =
    fun next msg ->
      Message.setContext KnownLiterals.HostContextName (Dns.GetHostName()) msg
      |> next

  /// Sets the service name as a context value
  [<CompiledName "Service">]
  let service (name: string): Middleware =
    fun next msg ->
      Message.setContext KnownLiterals.ServiceContextName name msg
      |> next

  let private pn = Process.GetCurrentProcess()

  /// Sets the current process' name as a context value
  [<CompiledName "ProcessName">]
  let processName: Middleware =
    fun next msg ->
      Message.setContext "processName" pn.ProcessName msg
      |> next

  /// Sets the current process' id as a context value
  [<CompiledName "ProcessId">]
  let processId: Middleware =
    fun next msg ->
      Message.setContext "processId" pn.Id msg
      |> next

  /// Always sets this context value.
  [<CompiledName "Context">]
  let context name value =
    fun next msg ->
      Message.setContext name value msg
      |> next

  [<CompiledName "AmbientSpanId">]
  let ambientSpanId: Middleware =
    fun next msg ->
      let ambientSpanId = Span.getActiveSpanId ()
      match ambientSpanId with
      | Some ambientSpanId -> msg |> Message.setSpanId ambientSpanId |> next
      | None -> next msg

  /// Compose the list of middlewares together into a single Message->Message function.
  [<CompiledName "Compose">]
  let compose: Middleware list -> Message -> Message = function
    | [] ->
      id
    | middlewares ->
      List.foldBack (fun f composed -> f composed) middlewares id
