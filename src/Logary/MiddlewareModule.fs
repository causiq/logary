namespace Logary

open System.Net

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Middleware =

  /// This is the identity middleware.
  [<CompiledName "Identity">]
  let identity : Middleware =
    fun next msg -> next msg

  /// Sets the host name as a context value
  [<CompiledName "Host">]
  let host : Middleware =
    let value = String (Dns.GetHostName())
    fun next msg ->
      Message.setContextValue "host" value msg
      |> next

  /// Sets the service name as a context value
  [<CompiledName "Service">]
  let service (name : string) : Middleware =
    let value = String name
    fun next msg ->
      Message.setContextValue "service" value msg
      |> next

  let private pn = Process.GetCurrentProcess()

  /// Sets the current process' name as a context value
  [<CompiledName "ProcessName">]
  let processName : Middleware =
    fun next msg ->
      Message.setContext "processName" pn.ProcessName msg
      |> next

  /// Sets the current process' id as a context value
  [<CompiledName "ProcessId">]
  let processId : Middleware =
    fun next msg ->
      Message.setContext "processId" pn.Id msg
      |> next

  /// Compose the list of middlewares together into a single Message->Message function.
  [<CompiledName "Compose">]
  let compose : Middleware list -> Message -> Message = function
    | [] ->
      id
    | middlewares -> 
      List.foldBack (fun f composed -> f composed) middlewares id