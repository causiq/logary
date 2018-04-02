module Logary.Targets.OpsGenie

open Hopac
open Hopac.Infixes
open Logary
open Logary.Message
open Logary.Target
open Logary.Formatting
open Logary.Internals
open Logary.Internals.Chiron
open Logary.Internals.Chiron.Operators
open HttpFs.Client

type CreateAlert =
  { apiKey: string
    message: string
    teams: string list option
    alias: string option
    description: string option
    recipients: string list option
    actions: string list option
    source: string option
    tags: string list option
    details: Map<string, Json> option
    entity: string option
    user: string option
    note: string option }

  static member ToJson (ca: CreateAlert) =
    Json.write "apiKey" ca.apiKey
    *> Json.write "message" ca.message
    *> Json.maybeWrite "teams" ca.teams
    *> Json.maybeWrite "alias" ca.alias
    *> Json.maybeWrite "description" ca.description
    *> Json.maybeWrite "recipients" ca.recipients
    *> Json.maybeWrite "actions" ca.actions
    *> Json.maybeWrite "source" ca.source
    *> Json.maybeWrite "tags" ca.tags
    *> Json.maybeWrite "details" ca.details
    *> Json.maybeWrite "entity" ca.entity
    *> Json.maybeWrite "user" ca.user
    *> Json.maybeWrite "note" ca.note

let ( **>) m1 m2 =
  Value.map2 (fun _ x -> x) m1 m2

type AlertCreated =
  { message: string
    alertId: string
    status: string
    code: int }
  static member FromJson (_ : AlertCreated) =
      fun m a s c -> { message = m; alertId = a; status = s; code = c }
    <!> Json.read "message"
    <*> Json.read "alertId"
    <*> Json.read "status"
    <*> Json.read "code"
  static member ToValue (ac : AlertCreated) =
    Value.write "message" ac.message
    **> Value.write "alertId" ac.alertId
    **> Value.write "status" ac.status
    **> Value.write "code" ac.code

let (|Val|_|) key o =
  Map.tryFind key o

let defaultMap token (message : Message) =
  let event' =
    match message.value with
    | Event e -> e
    | _ -> failwith "Events only to mixpanel at the moment"
  let details =
    match message |> Json.serialize with
    | Json.Object m -> m
    | _ -> failwith "Invalid message serialization in OpsGenie"
  {
    apiKey      = token
    message     = event'
    teams       = None
    alias       = Some event'
    description = None
    recipients  = None
    actions     = None
    source      = None
    tags        = None
    details     = Some details
    entity      = None
    user        = None
    note        = None
  }

type Token = string

type OpsGenieLogaryConf =
  { token : Token
    map   : Token -> Message -> CreateAlert
    url   : string }

  static member create(token, ?map, ?url) =
    { token = token
      map   = defaultArg map defaultMap
      url   = defaultArg url "http://api.opsgenie.com/v1/json/alert" }

let empty = OpsGenieLogaryConf.create ""

module internal Impl =
  let loop (conf : OpsGenieLogaryConf)
           (ri : RuntimeInfo)
           (requests : RingBuffer<TargetMessage>)
           (shutdown : Ch<IVar<unit>>)
           : Job<unit>=

    let rec loop () : Job<unit> =
      Alt.choose [
        shutdown ^=> fun ack ->
          ack *<= ()

        RingBuffer.take requests ^=> function
          | Log (logMsg, ack) ->
            job {
              match logMsg.value with
              | Event _ ->
                let payload =
                  logMsg
                  |> conf.map conf.token
                  |> Json.serialize
                  |> Json.format
                let request =
                  Request.createUrl Post conf.url
                  |> Request.bodyString payload
                let! (resp : AlertCreated) =
                            Request.responseAsString request
                            |> Job.map (Json.parse >> Json.deserialize)
                if resp.code > 201 then
                  do! eventX "Event rejected by OpsGenie"
                      >> setField "response" resp
                      |> ri.logger.errorWithBP
              | _ ->
                do! eventX "OpsGenie target should only be sent events, not metrics"
                    |> ri.logger.errorWithBP

              do! ack *<= ()
              return! loop ()
            }

          | Flush (ack, nack) ->
            Alt.choose [
              Ch.give ack ()
              nack :> Alt<_>
            ] ^=> loop
            :> Job<_>

      ] :> Job<_>

    loop ()

/// Create a new OpsGenie target
let create conf = TargetUtils.stdNamedTarget (Impl.loop conf)

/// Use with LogaryFactory.New( s => s.Target<OpsGenie.Builder>() )
type Builder(conf : OpsGenieLogaryConf, callParent : FactoryApi.ParentCallback<Builder>) =
  new(callParent : FactoryApi.ParentCallback<_>) =
    Builder(empty, callParent)

  member x.Token(token : Token) =
    Builder({ conf with token = token }, callParent)

  member x.Serializer(map : System.Func<string, Message, CreateAlert>) =
    Builder({ conf with map = fun t m -> map.Invoke(t, m) }, callParent)

  member x.Url(url: string) =
    Builder({ conf with url = url }, callParent)

  member x.Done () =
    ! (callParent x)

  interface Logary.Target.FactoryApi.SpecificTargetConf with
    member x.Build name = create conf name
