module Logary.Middleware

open System.Net
open Logary
open Logary.Utils.Aether

/// The type-signature for middleware; next:(Message -> Message) -> Message -> Message.
type Mid =
  (Message -> Message) -> Message -> Message

let identity : Mid =
  fun next msg -> next msg

let host : Mid =
  let hn = Value.String (Dns.GetHostName())

  fun next msg ->
    msg
    |> Message.setContextValue "host" hn
    |> next

let service (name : string) : Mid =
  let value = String name

  fun next msg ->
    msg
    |> Message.setContextValue "service" value
    |> next

let compose : Mid list -> Message -> Message = function
  | [] ->
    id

  | middlewares ->
    List.foldBack (fun f composed -> f composed) middlewares id