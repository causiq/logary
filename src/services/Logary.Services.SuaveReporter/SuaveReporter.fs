module Logary.Services.SuaveReporter

open System.Text
open Suave
open Suave.Model
open Suave.Filters
open Suave.RequestErrors
open Suave.Successful
open Suave.Operators

open Logary

module Impl =
  open NodaTime
  open NodaTime.Text

  open System
  open Aether
  open Aether.Operators
  open Chiron
  open Chiron.Operators
  open Suave.Operators
  
  type internal Random with
    /// generate a new random ulong64 value
    member x.NextUInt64() =
      let buffer = Array.zeroCreate<byte> sizeof<UInt64>
      x.NextBytes buffer
      BitConverter.ToUInt64(buffer, 0)

  let internal r = Random()

  type Message =
    { message : string
      id      : uint64 }

    static member ToJson (m : Message) : Json<unit> =
      Json.write "message" m.message
      *> Json.write "id" m.id

    static member FromJson (_ : Message) : Json<Message> =
      (fun m id -> { message = m
                     id      = id |> Option.fold (fun s t -> t) 0UL })
      <!> Json.read "message"
      <*> Json.tryRead "id"

  let jsonMsg msg =
    { message = msg; id = r.NextUInt64() }
    |> Json.serialize
    |> Json.format

  let jsonOK msg : WebPart =
    OK (jsonMsg msg) >=> Writers.setMimeType "application/json"

  let rec jsonToClr = function
    | Json.String s -> box s
    | Json.Number d -> box d
    | Json.Array arr -> box (List.map jsonToClr arr)
    | Json.Bool b -> box b
    | Json.Null _ -> box None
    | Json.Object m -> box (m |> Map.map (fun k v -> jsonToClr v))

  let readLogline (input : string) =
    let o =
      Json.Object_

    let root prop =
      o >?> Aether.Optics.Map.key_ prop

    let errors = 
      root "errors"
      >?> Json.Array_

    let message =
      errors
      >?> Aether.Optics.List.head_
      >?> Json.Object_
      >?> Aether.Optics.Map.key_ "message"
      >?> Json.String_

    let tryParseTs =
      InstantPattern.GeneralPattern.Parse
      >> (fun may -> if may.Success then Some may.Value else None)

    let timestamp = root "timestamp" >?> Json.String_
    let data      = root "data" >?> Json.Object_
    let session   = root "session" >?> Json.Object_
    let context   = root "context" >?> Json.Object_
    let tags      = root "tags" >?> Json.Array_
    let level     = root "level" >?> Json.String_

    let json = Json.parse input

    { message       = Lens.getPartialOrElse message "js log" json
      data          =
        Map [ "context",
                Lens.getPartialOrElse context Map.empty json
                |> Map.map (fun k v -> jsonToClr v)
                |> box
              "session",
                Lens.getPartialOrElse session Map.empty json
                |> Map.map (fun k v -> jsonToClr v)
                |> box
              "data",
                Lens.getPartialOrElse data Map.empty json
                |> Map.map (fun k v -> jsonToClr v)
                |> box
              "errors",
                Lens.getPartialOrElse errors List.empty json
                |> List.map (fun v -> jsonToClr v)
                |> box
            ]
      level         =
        Optic.get level json
        |> Option.fold (fun s t -> LogLevel.FromString t) LogLevel.Error
      tags          =
        Optic.getOrElse tags [] json
        |> List.map (fun json -> json, Json.String_)
        |> List.map (fun (json, lens) -> Optic.get lens json)
        |> List.filter Option.isSome
        |> List.map Option.get
      timestamp     = 
        Optic.get timestamp json
        |> Option.bind tryParseTs
        |> Option.fold (fun s t -> t)
                       (SystemClock.Instance.Now)
      path          = ""
      ``exception`` = None }

open Impl

let api (logger : Logger) (verbatimPath : string option) =
  let verbatimPath = defaultArg verbatimPath "/i/logary/loglines"
  let getMsg = sprintf "You can post a JSON structure to: %s" verbatimPath

  path verbatimPath >=> choose [
    GET >=> jsonOK getMsg
    POST >=> request (fun r ->
      let data = Encoding.UTF8.GetString(r.rawForm)
      Logger.log logger (readLogline data)
      CREATED (jsonMsg "Created")
    )
  ]