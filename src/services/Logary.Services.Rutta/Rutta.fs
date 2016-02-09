module Logary.Services.Rutta
open System.Reflection
[<assembly: AssemblyTitle("Logary Rutta – a router/proxy/shipper for Windows and Unix")>]
()

(* Specification:

This software should be capable of shipping logs to some arbitrary target.

v1: Hard-coded supported target types. Initially we'll just support InfluxDB.
v2: More configurable target configuration that supports any target.

This service can run in three modes; Shipper, Router and Proxy. Servers can be
implemented using Hopac's lightweight servers. Communication is implemented
using ZMQ and a binary serialisation format.

Bindings look may look like this:

  - `Shipper -> Router`
  - `Shipper -> Proxy`
  - `Proxy -> Proxy`
  - `Proxy -> Router`

[ZMQ socket reference](http://api.zeromq.org/3-2:zmq-socket)

The Shipper – from environment to Proxy or Router
-------------------------------------------------

Enables log shipping from hosts that are not directly connected to the router
nor to InfluxDB.

Should be spawnable on Unix. Should be service-installable on Windows using
TopShelf.

### Pushing Shippers

Shippers CONNECT PUSH sockets to the Router's PULL socket.
See http://lists.zeromq.org/pipermail/zeromq-dev/2012-February/015917.html

``` bash
./rutta --push-to tcp://headnode:6111
```

During network splits, the sending
[PUSH socket blocks](http://api.zeromq.org/3-2:zmq-socket#toc14).

### Publishing Shippers

``` bash
./rutta --pub-to tcp://headnode:7111
```

During network splits, the sending XPUSH socket drops messages.

The Proxy – from Shipper to Router
----------------------------------

Proxies take inputs from Shippers or other Proxies which publish Messages
using XPUB sockets:

``` bash
./rutta --pub-to tcp://headnode:7111
```

The Proxy is run this way, by providing a XSUB socket binding and a XPUB socket
binding:

``` bash
./rutta --proxy tcp://10.42.0.1:7111 tcp://192.168.10.10:7112
```

During network splits, the receiving
[XSUB socket drops messages](http://api.zeromq.org/3-2:zmq-socket#toc12).

You can then connect to the Proxy with a Router that routes it to the final
Target (like InfluxDB in this example):

``` bash
./rutta --router-sub tcp://192.168.10.10:7113 \
        --router-target influxdb://user:pass@host:8086/write?db=databaseName
```

During network splits, the sending
[XPUB socket drops messages](http://api.zeromq.org/3-2:zmq-socket#toc11).

The Router – from Shipper or Proxy to Target
--------------------------------------------

Implements Fan-In using PULL or SUB of Messages from ZMQ. Forwards internally
to a Target.

V1 only implements the InfluxDB target.

### Pulling Routers

BINDs a PULL socket on a specified NIC/IP and PORT.
Configures a single internal Target that pushes the received data.

``` bash
./rutta --router tcp://192.168.10.10:7113 \
        --router-target influxdb://user:pass@host:8086/write?db=databaseName
```

During network splits, the listening
[PULL socket blocks](http://api.zeromq.org/3-2:zmq-socket#toc15).

Serialisation
-------------

Should use a fast (network/memory/CPU) serialisation format, preferrably one
that supports zero copy.

The two competitors are:

 - [FsPickler](https://nessos.github.io/FsPickler/tutorial.html#Picklers-and-Pickler-combinators)
 - [MessagePack](https://github.com/msgpack/msgpack)

Each ZMQ message contains a Message (see DataModel.fs) in the binary form
given by the serialiser chosen.

Health Monitoring
-----------------

Rutta is itself a service that should be
[health monitored](https://www.consul.io/docs/agent/checks.html), and by default
it binds to `http://127.0.0.1:8888` for `GET /health` requests. All other
requests close the socket immediately. You can customise how the health binding
happens:

``` bash
./rutta <stuff> --health [::2144:1]:8888
```

or

``` bash
./rutta <stuff> --health 192.168.42.144:8888
```

It will respond with `200 OK`, otherwise `503 Service Unavailable`.

You can configure Consol to health check Rutta:

``` json
{
  "check": {
    "id": "api",
    "name": "HTTP API on port 8888",
    "http": "http://127.0.0.1:8888/health",
    "interval": "5s",
    "timeout": "500ms"
  }
}
```

*)

open Argu

type Args =
  | [<PrintLabels>] Push_To of pushConnectToSocket:string
  | [<PrintLabels>] Pub_To of pubBindSocket:string
  | [<PrintLabels>] Router of pullBindSocket:string
  | [<PrintLabels>] Router_Sub of subConnectSocket:string
  | [<PrintLabels>] Router_Target of logaryTargetUri:string
  | [<PrintLabels>] Health of ip:string * port:int
  | [<PrintLabels>] Proxy of xsubConnectToSocket:string * xpubBindSocket:string
with
  interface IArgParserTemplate with
    member s.Usage =
      match s with
      | Push_To _ -> "Runs Rutta in Shipper/PUSH mode (send Messages from a node to router)"
      | Pub_To _ -> "Runs Rutta in Shipper/PUB mode (send Messages from a node to proxy)"
      | Router _ -> "Runs Rutta in Router mode (PULL fan-in of Messages, forward to Target)."
      | Router_Sub _ -> "Runs Rutta in Router XSUB mode for PUB sockets to publish to."
      | Router_Target _ -> "Implied by --router. Specifies where the Router target should forward its data"
      | Health _ -> "Give Rutta binding information"
      | Proxy (_,_) -> "Runs Rutta in Proxy mode (XSUB/fan-in of Messages, forward to SUB sockets via XPUB). The first is the XSUB socket (in), the second is the XPUB socket (out)."

module Health =
  open Logary
  open System
  open System.Threading
  open Suave
  open Suave.Operators
  open Suave.Filters
  open Suave.Successful

  let app =
    GET >=> path "/health" >=> OK (sprintf "Logary Rutta %s" (App.getVersion ()))

  /// Start a Suave web server on the passed ip and port and return a disposable
  /// token to use to shut the server down.
  let startServer ip port =
    let cts = new CancellationTokenSource()

    let config =
      { defaultConfig with
          bindings = [ HttpBinding.mkSimple HTTP ip port ]
          cancellationToken = cts.Token }

    let ready, handle = startWebServerAsync config app
    Async.Start handle

    { new IDisposable with member x.Dispose() = cts.Cancel () }

module Router =
  ()

module Shipper =
  ()

module Proxy =
  ()

open System
open System.Threading
open Topshelf

let startUnix argv =
  let parser = ArgumentParser.Create<Args>()
  let parsed = parser.Parse argv
  use exiting = new ManualResetEventSlim(false)
  use sub = Console.CancelKeyPress.Subscribe(fun _ -> exiting.Set())
  use health =
    parsed.GetResult(<@ Health @>, defaultValue = ("127.0.0.1", 8888))
    ||> Health.startServer

  let start =
    parsed.GetAllResults None
    |> List.fold (function
    | Choice1Of2 (_, pars) -> function
      |
    | Choice2Of2 pars -> function
      | Push_To _ ->
      | Pub_To _ -> "Runs Rutta in Shipper/PUB mode (send Messages from a node to proxy)"
      | Router _ -> "Runs Rutta in Router mode (PULL fan-in of Messages, forward to Target)."
      | Router_Sub _ ->
      | Router_Target _ ->
      | Health _ ->
      | Proxy (_,_) ->
    ) (Choice2Of2 []) // Choice2Of2 = failure to find a meaningful 

  exiting.Wait()
  0

let startWindows argv =
  let exiting = new ManualResetEventSlim(false)

  let enqueue f =
    ThreadPool.QueueUserWorkItem(fun _ -> f ()) |> ignore

  let start hc =
    enqueue <| fun _ ->
      // TODO

      exiting.Wait()
    true

  let stop hc =
    exiting.Dispose()
    true

  Service.Default
  |> with_recovery (ServiceRecovery.Default |> restart (Time.s 5))
  |> with_start start
  |> with_stop (fun hc -> exiting.Set() ; stop hc)
  |> run

[<EntryPoint>]
let main argv =
  if Type.GetType "Mono.Runtime" <> null then startUnix argv else startWindows argv