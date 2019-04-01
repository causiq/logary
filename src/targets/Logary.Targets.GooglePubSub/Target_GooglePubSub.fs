module Logary.Targets.GooglePubSub

open System.Runtime.CompilerServices
open Google.Api.Gax
open Google.Cloud.PubSub.V1
open Hopac
open Hopac.Infixes
open NodaTime
open Logary
open Logary.Message
open Logary.Internals
open Logary.Configuration.Target

[<assembly:InternalsVisibleTo("Logary.Targets.GooglePubSub.Tests")>]
do()

type TopicSelector =
  | Constant of topic:string
  | Selector of selector:(Message -> string)

type GooglePubSubConf =
  { projectId: string option
    topic: TopicSelector
    shutdownTimeout: Duration
    pubSettings: PublisherServiceApiSettings
  }

  member x.topicFor (m: Message) =
    match x.topic with
    | Constant topic ->
      topic
    | Selector selector ->
      selector m

  /// https://googleapis.github.io/google-cloud-dotnet/docs/Google.Cloud.PubSub.V1/api/Google.Cloud.PubSub.V1.PublisherServiceApiSettings.html
  static member create (?projectId: string, ?topic: TopicSelector, ?shutdownTimeout: Duration, ?pubSettings: PublisherServiceApiSettings) =
    { projectId = projectId
      topic = topic |> Option.defaultValue (Constant "logs")
      shutdownTimeout = defaultArg shutdownTimeout (Duration.FromSeconds 5L)
      pubSettings = pubSettings |> Option.defaultWith PublisherServiceApiSettings
    }

let empty = GooglePubSubConf.create()

/// https://googleapis.github.io/google-cloud-dotnet/docs/Google.Cloud.PubSub.V1/
module internal Impl =
  open System.Threading.Tasks
  open Google.Protobuf
  open Logary.Formatting
  open Logary.Internals.Chiron

  type Message with
    member x.toPubSub() =
      let psm = new PubsubMessage()
      psm.MessageId <- "" // TODO: hash message contents
      // TODO: configurable attributes
      // TODO: unit test the removal of all _logary.-prefixes when serialising the message to JSON
      // TODO: unit test the creation of a gauge from JSON
      let json = Json.encode x |> Json.formatWith JsonFormattingOptions.Compact
      psm.Data <- ByteString.CopyFromUtf8 json
      // psm.Attributes.Add // TO CONSIDER: more attributes
      psm

  type PublisherClient with
    member x.publish (m: PubsubMessage) =
      Alt.fromTask (fun _ -> x.PublishAsync m)

  type Topic = string

  type State =
    { projectId: string
      clients: Map<Topic, PublisherClient>
    }

    /// Gets the client for the given topic. The reason this function exists, is that the topic can be dynamically
    /// decided based on the message contents.
    member x.clientFor (conf: GooglePubSubConf) (topic: Topic): Job<State * PublisherClient> =
      match x.clients |> Map.tryFind topic with
      | Some c ->
        Job.result (x, c)
      | None ->
        let tn = new TopicName(x.projectId, topic)
        job {
          let! publisherService = PublisherServiceApiClient.CreateAsync(settings=conf.pubSettings)
          let! client = PublisherClient.CreateAsync(tn (* and ClientCreationSettings and PublisherClient.Settings *))
          let _ = publisherService.CreateTopic(tn (* and call settings *))
          let nextState = { x with clients = x.clients |> Map.add topic client }
          return nextState, client
        }

    /// Shuts down all the clients in this state.
    member x.shutdown() =
      Alt.fromUnitTask <| fun ct ->
      x.clients
      |> Seq.map (fun (KeyValue (_, client)) -> client.ShutdownAsync(ct))
      |> Seq.toArray
      |> Task.WhenAll

  let getProjectId (conf: GooglePubSubConf) =
    conf.projectId
    |> Option.map Job.result
    |> Option.defaultWith (fun _ -> job { let! platform = Platform.InstanceAsync()
                                          return platform.ProjectId })

  /// The main target loop
  let loop (conf: GooglePubSubConf) (api: TargetAPI) =
    let logger = api.runtime.logger

    let rec initialise () =
      job {
        let! projectId = getProjectId conf
        let initialState = { projectId=projectId; clients=Map.empty }
        match conf.topic with
        | Constant tn ->
          let! nextState, _ = initialState.clientFor conf tn
          return! running nextState
        | _ ->
          return! running initialState
      }

    and running (state: State): Job<_> =
      Alt.choose [
        RingBuffer.take api.requests ^=> function
          | TargetMessage.Log (m, a) ->
            job {
              let topic = conf.topicFor m
              let! stateNext, client = state.clientFor conf topic
              let message = m.toPubSub()
              let! messageId = client.publish message
              logger.verbose (eventX "Got ack of {messageId}" >> setField "messageId" messageId)
              do! a *<= ()
              return! running state
            }

          | Flush (a, n) ->
            a *<= () >>=. running state

        api.shutdownCh ^=> fun ack ->
          timeOut (conf.shutdownTimeout.toTimeSpanSafe()) <|> state.shutdown()
          >>=. ack *<= ()
      ] :> Job<_>

    initialise ()

/// Create a new Google Pub/Sub target.
/// https://cloud.google.com/pubsub/docs/overview
[<CompiledName "Create">]
let create conf name =
  TargetConf.createSimple (Impl.loop conf) name

open System

/// Use with LogaryFactory.New(s => s.Target<BigQuery>())
type Builder(conf, callParent: ParentCallback<Builder>) =
  let update (conf': GooglePubSubConf): Builder =
    Builder(conf', callParent)

  member x.ProjectId(projectId: string) =
    update { conf with projectId = Some projectId }

  member x.TopicSelector (getTopic: Func<Message, string>) =
    update { conf with topic = Selector (fun m -> getTopic.Invoke m) }

  member x.Topic (topic: string) =
    update { conf with topic = Constant topic }

  member x.ShutdownTimeout (d: Duration) =
    update { conf with shutdownTimeout = d }

  member x.PubSettings (ps: PublisherServiceApiSettings) =
    update { conf with pubSettings = ps }

  new(callParent: ParentCallback<_>) =
    Builder(empty, callParent)

  interface SpecificTargetConf with
    member x.Build name = create conf name
