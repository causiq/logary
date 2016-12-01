module Logary.Targets.RabbitMQ

open System
open NodaTime
open Hopac
open Hopac.Infixes
open Logary
open Logary.Message
open Logary.Target
open Logary.Internals
open RabbitMQ.Client
open RabbitMQ.Client.Framing

type Compression =
  | NoCompression
  | GZip

type DeliveryMode =
  | NonPersistent = 1uy
  | Persistent = 2uy

type Port = uint16

type TlsConf =
  { /// The location of the TLS certiface
    certPath : string
    /// The password to the TLS certificate (pfx formatted)
    certPassword : string option }

/// This is the configuration for the RabbitMQ target
type RabbitMQConf =
  { /// Sets the virtual host to send messages to.
    /// Defaults to '/'
    vHost    : string

    /// Sets the username for the connection. Defaults to 'guest'
    username : string

    /// Sets the password for the connection. Default to 'guest'.
    password : string

    /// Sets the AMQP protocol (version) to use
    /// for communications with the RabbitMQ broker. The default 
    /// is the RabbitMQ.Client-library's default protocol.
    /// Default is the latest supported.
    protocol : IProtocol

    /// Sets the host name of the broker to log to.
    hostname : string 

    ///	Sets the port to use for connections to the message broker 
    /// (this is the broker's listening port). The default is '5672'. 
    port     : Port

    ///	Sets the routing key (aka. topic) with which
    ///	to send messages. Defaults to {0}, which in the end is 'error' for log.Error("..."), and
    ///	so on. An example could be setting this property to 'ApplicationType.MyApp.Web.{0}'.
    ///	The default is '{0}'.
    topic    : string

    /// Sets the exchange to bind the logger output to. Default is 'logary'.
    exchange : string

    /// Sets the exchange type to bind the logger output to. Default is 'topic'. Also see
    /// `ExchangeType`.
    exchangeType : string

    /// Sets the setting specifying whether the exchange
    ///	is durable (persisted across restarts). Default is true.
    durable : bool

    /// Sets the setting specifying whether the exchange should be declared or used passively.
    /// Defaults to false.
    passive : bool

    /// Gets or sets the application id to specify when sending. Defaults to None,
    /// and then IBasicProperties.AppId will be the name of the logger instead.
    appId : string option

    /// Optional TLS configuration
    tls : TlsConf option

    /// Persistent or non-persistent. Default to Persistent (2).
    deliveryMode : DeliveryMode

    /// How long to wait for a connection before failing the initialisation
    connectionTimeout : Duration

    /// Compression method to use. Defaults to None
    compression : Compression
  }

/// Default RabbitMQ config
let empty =
  { vHost             = "/"
    username          = "guest"
    password          = "guest"
    hostname          = "localhost"
    port              = 5672us
    topic             = "{0}"
    protocol          = Protocols.DefaultProtocol
    exchange          = "logary"
    exchangeType      = ExchangeType.Topic
    durable           = true
    passive           = false
    appId             = None
    tls               = None
    deliveryMode      = DeliveryMode.Persistent
    connectionTimeout = Duration.FromSeconds 10L
    compression       = NoCompression }

module internal Impl =
  open Logary.YoLo
  open Logary.Utils.Chiron

  module Counter =
    open System
    open System.Threading

    let mutable private counter = 0L

    let next () =
      Interlocked.Increment(& counter) |> uint64

  type MessageId = uint64

  /// State holder for the RabbitMQ target
  type State =
    { connection : IConnection
      // aka Channel
      model      : IModel
      // https://www.rabbitmq.com/dotnet-api-guide.html#common-patterns – see section on publisher
      // confirms
      // Also https://www.rabbitmq.com/confirms.html
      inflight   : Map<MessageId, IVar<unit> * Message>
      // https://www.rabbitmq.com/releases/rabbitmq-dotnet-client/v3.6.5/rabbitmq-dotnet-client-3.6.5-client-htmldoc/html/namespace-RabbitMQ.Client.Events.html
      acks       : Stream<Events.BasicAckEventArgs>
      nacks      : Stream<Events.BasicNackEventArgs>
      /// Last uint64 value received on nack or ack channels
      lastAck    : MessageId }

    interface IDisposable with
      member x.Dispose () =
        x.model.Close(200us, "Client shutting down")
        x.connection.Dispose()

  let createConnection (clientName : string) (conf : RabbitMQConf) =
    let tls =
      conf.tls |> Option.fold (fun s t ->
        let pass = t.certPassword |> Option.fold (fun s t -> t) null
        SslOption(conf.hostname, t.certPath, true, CertPassphrase = pass)
      ) (SslOption())

    let fac =
      ConnectionFactory(
        HostName = conf.hostname,
        VirtualHost = conf.vHost,
        UserName = conf.username,
        Password = conf.password,
        RequestedHeartbeat = 60us,
        AutomaticRecoveryEnabled = true,
        NetworkRecoveryInterval = TimeSpan.FromSeconds 30.,
        TopologyRecoveryEnabled = true,
        Port = int conf.port,
        Ssl = tls,
        RequestedConnectionTimeout = int (conf.connectionTimeout.ToTimeSpan().TotalMilliseconds)
      )

    let conn = fac.CreateConnection(clientName)
    conn.AutoClose <- false // closed when disposed
    conn

  let createModel (conn : IConnection) =
    let model = conn.CreateModel()
    model.ConfirmSelect()
    let acks = Stream.Src.create ()
    model.BasicAcks.Add(Stream.Src.value acks >> start)
    let nacks = Stream.Src.create ()
    model.BasicNacks.Add(Stream.Src.value nacks >> start)
    model, Stream.Src.tap acks, Stream.Src.tap nacks

  module Map =
    /// Try-find the item and also remove it if found.
    let pop k m =
      match m |> Map.tryFind k with
      | None ->
        m, None

      | Some x ->
        m |> Map.remove k,
        Some x

  let topic (conf : RabbitMQConf) (message : Message) =
    conf.topic.Replace("{0}", message.level.ToString())

  let props (model : IModel) (conf : RabbitMQConf) (message : Message) =
    let props = model.CreateBasicProperties()
    props.AppId <- conf.appId |> Option.fold (fun s t -> t) (PointName.format message.name)
    props.ContentEncoding <- "utf8"
    props.ContentType <- "application/json; charset=utf-8"
    props.Timestamp <-
      AmqpTimestamp((* assume in seconds since epoch *) message.timestamp / 1000000L)
    props.UserId <- conf.username
    props.DeliveryMode <- byte conf.deliveryMode
    let msgId = Counter.next () // this generates the delivery tag in a sequential manner
    props.MessageId <- string msgId
    props, msgId

  let compress = function
    | NoCompression ->
      id
    | GZip ->
      fun bytes ->
        let ms = new IO.MemoryStream()
        use gz = new IO.Compression.GZipStream(ms, IO.Compression.CompressionMode.Compress)
        gz.Write(bytes, 0, bytes.Length)
        ms.ToArray()

  let body (conf : RabbitMQConf) (message : Message) =
    Json.serialize message |> Json.format |> UTF8.bytes |> compress conf.compression

  let selectConfirm (ilogger : Logger) state (kont : State -> Job<unit>) : Alt<_> =

    let clearInflights (m, acked) msgId =
      match m |> Map.pop msgId with
      | m', Some (iAck, msg) ->
        m', (iAck, msg) :: acked
      | m', None ->
        m', acked

    let ackSingle inflight deliveryTag =
      match state.inflight |> Map.pop deliveryTag with
      | inflight', Some (iAck, msg) ->
        iAck *<= () |> Job.map (fun _ -> inflight')

      | inflight', None ->
        Job.result inflight'

    let ackMany state deliveryTag =
      let inflight', acked =
        [ state.lastAck .. state.lastAck + deliveryTag ]
        |> List.fold clearInflights (state.inflight, [])

      // ack callers inside process:
      acked
      |> List.map (fst >> fun iAck -> IVar.fill iAck ())
      |> Job.conCollect
      |> Job.map (fun _ -> inflight')

    Alt.choose [
      Stream.values state.nacks ^=> fun nack ->
        // TO CONSIDER: handle nacks somehow...
        // Right now we'll just remove the message from the list of inflight messages
        // Some googling should be needed for the exact semantics – but most likely it's
        // failing because you have no consumers configured.
        ilogger.infoWithBP (
          eventX "Got {nack}."
          >> setField "deliveryTag" nack.DeliveryTag
          >> setField "multiple" nack.Multiple)
        |> Job.bind (fun _ -> job {
          if nack.Multiple then
            let! inflight' = ackMany state nack.DeliveryTag
            return! kont { state with inflight = inflight' }
          else
            let! inflight' = ackSingle state.inflight nack.DeliveryTag
            return! kont { state with inflight = inflight'
                                      lastAck = nack.DeliveryTag }
        })

      Stream.values state.acks ^=> fun ack ->
        job {
          if ack.Multiple then
            let! inflight' = ackMany state ack.DeliveryTag
            return! kont { state with inflight = inflight'
                                      lastAck  = ack.DeliveryTag }
          else
            let! inflight' = ackSingle state.inflight ack.DeliveryTag
            return! kont { state with inflight = inflight'
                                      lastAck  = ack.DeliveryTag }
        }
    ]

  let loop (conf : RabbitMQConf)
           (ri : RuntimeInfo)
           (requests : RingBuffer<_>)
           (shutdown : Ch<_>) =

    let rec connect () : Job<unit> =
      let conn = createConnection ri.serviceName conf
      let model, acks, nacks = createModel conn
      active { connection = conn
               model      = model
               acks       = acks
               nacks      = nacks
               inflight   = Map.empty
               lastAck    = 0UL }

    // in the active state we selectively accept ACK and NACKs from RabbitMQ whilst
    // consuming messages from our RingBuffer, one by one. RabbitMQ has its own
    // IO thread with its own queue, so the 'publish' operation doesn't mean anything
    // from a durability perspective; instead it is the publisher confirms that we've
    // enabled that matters (see `createModel`) – and the function `selectConfirm`
    and active (state : State) : Job<unit> =
      Alt.choose [
        selectConfirm ri.logger state active

        RingBuffer.take requests ^=> function
          | Log (message, ack) ->
            job {
              let topic = topic conf message
              let props, msgId = props state.model conf message
              let body = body conf message
              let inflight' = state.inflight |> Map.add msgId (ack, message)
              
              state.model.BasicPublish(conf.exchange, topic, props, body)
              return! active { state with inflight = inflight' }
            }

          | Flush (ackCh, nack) ->
            flushing (ackCh, nack) state

        // shutdown closes the connection and channel but does not flush
        shutdown ^=> fun ack ->
          job {
            do! Job.Scheduler.isolate <| fun _ ->
              Try.safe "RabbitMQ target disposing connection and model/channel"
                       ri.logger
                       (state :> IDisposable).Dispose
                       ()
            return! ack *<= ()
          }
      ] :> Job<_>

    // in the `flushing` state, we'll just select on the confirm channel until
    // the inflight messages list is empty
    and flushing (ackCh, nack) (state : State) =
      if Map.isEmpty state.inflight then
        job {
          do! Ch.give ackCh () <|> nack
          return! active state
        }
      else
        Alt.choose [
          nack ^=>. active state
          selectConfirm ri.logger state (flushing (ackCh, nack))
        ] :> Job<_>

    connect ()

/// Create a new RabbitMQ target.
[<CompiledName "Create">]
let create conf name = TargetUtils.stdNamedTarget (Impl.loop conf) name

// The Builder construct is a DSL for C#-people. It's nice for them to have
// a DSL where you can't make mistakes. The general idea is that first 'new'
// is called, and you get the callback to that function. Then you can put
// methods on this Builder class which are exposed to the caller (configuration
// code).

/// Use with LogaryFactory.New( s => s.Target<RabbitMQ.Builder>() )
type Builder(conf, callParent : FactoryApi.ParentCallback<Builder>) =
  let update conf' =
    Builder(conf', callParent)

  member x.VHost(vhost) =
    update { conf with vHost = vhost }

  member x.UserName username =
    update { conf with username = username }

  member x.Password pass =
    update { conf with password = pass }

  member x.Protocol proto =
    update { conf with protocol = proto }

  member x.Hostname hostname =
    update { conf with hostname = hostname }

  member x.Port port =
    update { conf with port = port }

  member x.Topic topic =
    update { conf with topic = topic }

  member x.Exchange exchange =
    update { conf with exchange = exchange }

  member x.ExchangeType typ =
    update { conf with exchangeType = typ }

  member x.DurableExchange () =
    update { conf with durable = true }

  member x.EphemeralExchange () =
    update { conf with durable = false }

  member x.Passive () =
    update { conf with passive = true }

  member x.Active () =
    update { conf with passive = false }

  member x.AppId app =
    if app = null then invalidArg "app" "must not be null"
    update { conf with appId = Some app }

  member x.EnableTls(path, nullablePass : string) =
    let pass = if nullablePass = null then None else Some nullablePass
    update { conf with tls = Some { certPath = path; certPassword = pass }}

  member x.PersistentDelivery () =
    update { conf with deliveryMode = DeliveryMode.Persistent }

  member x.NonPersistentDelivery () =
    update { conf with deliveryMode = DeliveryMode.NonPersistent }

  member x.ConnectionTimeout (dur : Duration) =
    update { conf with connectionTimeout = dur }

  member x.CompressGZip () =
    update { conf with compression = GZip }

  member x.Done() =
    ! (callParent x)

  // c'tor, always include this one in your code
  new(callParent : FactoryApi.ParentCallback<_>) =
    Builder(empty, callParent)

  // this is called in the end, after calling all your custom configuration
  // methods (above) which in turn take care of making the F# record that
  // is the configuration, "just so"
  interface Logary.Target.FactoryApi.SpecificTargetConf with
    member x.Build name = create conf name