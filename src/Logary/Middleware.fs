module Logary.Middleware

open System.Diagnostics
open System.Net
open Logary
open Logary.Utils.Aether

/// The type-signature for middleware; next:(Message -> Message) -> Message -> Message.
type Mid =
  (Message -> Message) -> Message -> Message

/// This is the identity middleware.
[<CompiledName "Identity">]
let identity : Mid =
  fun next msg -> next msg

/// Sets the host name as a context value
[<CompiledName "Host">]
let host : Mid =
  let value = String (Dns.GetHostName())
  fun next msg ->
    Message.setContextValue "host" value msg
    |> next

/// Sets the service name as a context value
[<CompiledName "Service">]
let service (name : string) : Mid =
  let value = String name
  fun next msg ->
    Message.setContextValue "service" value msg
    |> next

let private pn = Process.GetCurrentProcess()

/// Sets the current process' name as a context value
[<CompiledName "ProcessName">]
let processName : Mid =
  fun next msg ->
    Message.setContext "processName" pn.ProcessName msg
    |> next

/// Sets the current process' id as a context value
[<CompiledName "ProcessId">]
let processId : Mid =
  fun next msg ->
    Message.setContext "processId" pn.Id msg
    |> next

/// Compose the list of middlewares together into a single Message->Message function.
[<CompiledName "Compose">]
let compose : Mid list -> Message -> Message = function
  | [] ->
    id
  | middlewares -> 
    List.foldBack (fun f composed -> f composed) middlewares id