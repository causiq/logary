module Logary.Middleware

open System.Net
open Logary
open Logary.Utils.Aether

type Mid =
  (Message -> Message) -> Message -> Message

let identity : Mid =
  fun next msg -> next msg

let hostname : Mid =
  let hn = Value.String (Dns.GetHostName())

  fun next msg ->
    msg
    |> Message.setContextValue "hostname" hn
    |> next

let service (name : string) : Mid =
  let value = String name

  fun next msg ->
    msg
    |> Message.setContextValue "service" value
    |> next

type State<'s> =
  private { x : 's }

module StateModule =

  let dispatch state msg =
    msg

let compose : Mid list -> Message -> Message = function
  | [] ->
    id

  | middlewares ->
    List.foldBack (fun f composed -> f composed) middlewares id