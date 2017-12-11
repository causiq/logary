module Logary.Services.SuaveReporter

open System.Text
open Logary
open Logary.Internals.Aether
open Logary.Internals.Aether.Operators

module Impl =
  open NodaTime
  open NodaTime.Text
  open System
  open Suave

  type JsonMsg =
    { message : string
      id      : uint64 }

  //   static member ToJson (m : JsonMsg) : Json<unit> =
  //     Json.write "message" m.message
  //     *> Json.write "id" m.id

  //   static member FromJson (_ : JsonMsg) : Json<JsonMsg> =
  //     (fun m id -> { message = m
  //                    id      = id |> Option.fold (fun s t -> t) 0UL })
  //     <!> Json.read "message"
  //     <*> Json.tryRead "id"

open Impl
open Hopac
open Suave
open Suave.Model
open Suave.Filters
open Suave.RequestErrors
open Suave.Successful
open Suave.Operators
open Suave.Utils

let bind (f : 'a -> Choice<'b, 'c>) (v : Choice<'a, 'c>) =
  match v with
  | Choice1Of2 v -> f v
  | Choice2Of2 c -> Choice2Of2 c

let api (logger : Logger) (verbatimPath : string option) : WebPart =
  let verbatimPath = defaultArg verbatimPath "/i/logary"
  let getMsg = sprintf "You can post a JSON structure to: %s" verbatimPath

  let inline readJson () =
    failwith "TODO: needs to be discussed"
    // Lens.get HttpRequest.rawForm_
    // >> System.Text.Encoding.UTF8.GetString
    // >> Json.tryParse
    // >> bind Json.tryDeserialize

  let jsonMsg msg =
    // { message = msg; id = ThreadSafeRandom.nextUInt64 () }
    // |> Json.serialize
    // |> Json.format
    msg

  path verbatimPath >=> choose [
    GET >=> OK (jsonMsg getMsg)
    POST >=> Binding.bindReq
              (readJson ())
              (fun (msg:Message) ->
                Logger.logSimple logger msg
                CREATED (jsonMsg "Created"))
              BAD_REQUEST
  ]
