# Logary v4

## Why?

Logary is a high-performance, semantic logging, health and metrics library for
.Net.

These are some reasons why you should use Logary:

 - do semantic logging – combine with Kibana/Logstash
 - F# idiomatic logging code
 - `Logary.CSharp` for C# idiomatic logging code
 - doesn't throw exceptions (on your callers)
 - low latency logging, efficient use of resources
 - advanced `Rule`-based system supports hierarchical logging
 - you can create your own metrics and derived/computed metrics or;
 - you can treat events as a Gauge of 1, ship it to Influx and be done with it

Created by [Henrik Feldt, et al](https://haf.github.io) and sponsored by
[qvitoo – A.I. bookkeeping](https://qvitoo.com/?utm_source=github&utm_campaign=logary).

## Install it

paket.dependencies:

```
source https://www.nuget.org/api/v2
nuget Logary
```

OR:

```
Install-Package Logary
```

## Hello World

```fsharp
open System
open NodaTime
open Hopac
open Logary
open Logary.Configuration
open Logary.Targets
open Logary.Metric
open Logary.Metrics
open System.Threading

[<EntryPoint>]
let main argv =
  // the 'right' way to wait for SIGINT
  use mre = new ManualResetEventSlim(false)
  use sub = Console.CancelKeyPress.Subscribe (fun _ -> mre.Set())

  // create a new Logary; save this instance somewhere "global" to your app/service
  use logary =
    // main factory-style API, returns IDisposable object
    withLogaryManager "Logary.Examples.ConsoleApp" (
      // output to the console
      withTargets [
        Console.create (Console.empty) (PointName.ofSingle "console")
      ] >>
      // continuously log CPU stats
      withMetrics [
        MetricConf.create (Duration.FromMilliseconds 500L) (PointName.ofSingle "cpu") Sample.cpuTime
      ] >>
      // "link" or "enable" the loggers to send everything to the configured target
      withRules [
        Rule.createForTarget (PointName.ofSingle "console")
      ]
      // "compile" the configuration of targets, above
      >> run
    )
    // "compile" the Logary instance
    |> run

  // Get a new logger. Also see Logging.getLoggerByName for statically getting it
  let logger =
    logary.getLogger (PointName [| "Logary"; "Samples"; "main" |])

  // log something
  Message.event Info "User logged in"
  |> Message.setField "userName" "haf"
  |> Logger.logSimple logger

  // wait for sigint
  mre.Wait()
  0
```

## v4

**This is a pre-release version**. v3.x is at
[releases/v3.x](https://github.com/logary/logary/tree/releases/v3.x). We
consider the master branch stable at this point.

Logary is a high performance, multi-target logging, metric, tracing and
health-check library for mono and .Net and [JavaScript](https://github.com/logary/logary-js),
with a healthy dose of [documentation](https://logary.github.io/).

Follow Logary at twitter: [@logarylib](https://twitter.com/logarylib)

Chat and support and get support:
[![Gitter chat](https://badges.gitter.im/logary.png)](https://gitter.im/logary/logary)

Just add this to your paket.dependencies:

``` paket
nuget Logary
```

At [https://logary.github.io/](https://logary.github.io) you can find the full documentation.

## Overview

Logary is itself a library for metrics and events with extensible inputs, *adapters*, and
outputs, *targets*. Further, its *services* run as their own processes or in
[Suave](https://suave.io/?utm_source=github&utm_campaign=logary).

 - **Logary** – the main logging and metrics library. Your app depends on this.
 - Logary.CSharp - C# facade which makes it more *object oriented*.
 - Logary.Facade - single file to use in your F# library.
 - **Logary.Targets** (from *Logary* into DBs and monitoring infra):
   * DB – write logs into an arbitrary database: SQL Server, MySQL, PostgreSQL, sqlite and so on...
   * *DB.Migrations* – uses [FluentMigrator](https://github.com/schambers/fluentmigrator/)
     to create and then upgrade your DB between versions of Logary.
   * Heka – ships *Events* and *Metrics* into Heka.
   * InfluxDb – ships *Events* (as annotations) and *Metrics* into [InfluxDb](https://influxdata.com).
   * Logstash – ships *Events* and *Metrics* into [Logstash](https://www.elastic.co/products/logstash)
     [over](https://www.elastic.co/guide/en/logstash/current/plugins-inputs-zeromq.html)
     [ZeroMQ](http://zeromq.org/).
   * <span title="A transactional e-mail service that lets you send e-mail with code">Mailgun</span>
     – ships *Events* over e-mail – send yourself warnings, errors and fatal errors
     via [Mailgun](http://www.mailgun.com/).
   * <span title="The sharpest clojurian knife in the drawer for acting on metrics">Riemann</span>
     – ships *Events* (as a 1-valued gauage) and *Metrics* into [Riemann](http://riemann.io/).
   * Shipper – ships *Messages* (*Events*/*Metrics*) to the `Router` or `Proxy` (see `Rutta` above)
 - **Logary.Adapters** (from *X* into Logary):
   * <span title="Make yourself dependent on not just one, but two logging frameworks">CommonLogging</span>
     – *moar abstract* logs into Logary.
   * EventStore – [EventStore](https://geteventstore.com/) logs into Logary.
   * *Facade* –  receiver for `Logary.Facade` logs.
   * <span title="F# API for dealing with DBs">FsSql</span> –
     [FsSql](https://www.nuget.org/packages/FsSql/) logs into Logary.
   * <span title="A web authorisation library">Hawk</span> - Logibit's
     [Hawk](https://www.nuget.org/packages/Hawk/) logs into Logary.
   * log4net – lets log4net log into Logary.
   * <span title="Suave is a web server library that makes you happy">Suave</span> – ships
     [Suave](https://suave.io) to logs into Logary.
   * <span title="Topshelf is a Windows service host">Topshelf</span> – ships
     [Topshelf](https://www.nuget.org/packages/Topshelf/) logs into Logary.
 - **Logary.Metrics** (from *environment* into Logary):
   * WinPerfCounters – an API to access Windows Performance Counters.
 - **Logary.Services** (stand-alone functionality):
   * Rutta – a godly service of three:
     1. *Ships* Windows Performance Counters to the `Router` or `Proxy`, pushing via a PUB or PUSH ZeroMQ socket.
     2. *Proxies* `Messages` between the `Shipper` and the `Router`, listening on a ZeroMQ XSUB/XPUB socket.
     3. *Routes* `Messages` to Targets, listening on a ZeroMQ SUB or PULL socket.
     <blockquote>
       Note that the shipping feature is its own target as well. Why? So that you can send logs in an efficient,
       high-performance manner between machines, without going through a potentially destructure
       mapping to another serialisation format or through another log router (Heka, Logstash) which
       also may change your data structure.
     </blockquote>
   * SQLServerHealth – a service that keep track of IO/latency performance for highly loaded SQL Servers
   * SuaveReporter – a well-maintained Suave WebPart that you run as a part of your Suave
     server, which enables you to use [logary-js](https://www.npmjs.com/package/logary-js).

## Tutorial and Data Model

The core type is **`Message`** which is the smallest unit you can log. It has
three sorts of point values: `Event`, `Gauge` and `Derived`. An event is
normally a single line of code and carries a template string. E.g. "User
logged in" is an event's template string, and the `Message` would have a field
"user" => "haf".

### PointName

A point is a location where you send a metric or event from. Usually a module
and in mature projects you also often have the name of the function that you log
from as a part of the point name.

### PointValue.Event

What you expect: `"User logged in"` with a field `"userName"`, `"haf"`.

### PointValue.Gauge

An instantaneous value. Imagine the needle showing the speed your car is going
or a digital display showing the same instantaneous metric value of your car's
speed.

An event is the most simple gauge of value 1.

### PointValue.Derived

A derived value from one or many gauge.

### Rule & Hierarchical logging

It means that you can have one `Rule`/`Logger` at level `Info` for namespace
`MyCompany` and another `Rule` that matches loggers at `MyCompany.Submodule`
which allows Messages of level `Debug` to go through.

A normal use-case for this is when you want to debug a particular module, by
increasing the verbosity of its output (decreasing its log level).

### Log Level

The highest log level is `Fatal` which should be reserved for things that make
your service/process crash. Things like; "my disk is full and I'm a database
trying to start", or "I'm a 2-tier service built with a database, which I cannot
do any work without" warrant the `Fatal` level.

At this level human being are normally directly alerted.

The next level is `Error` which should be reserved for what you consider to be
edge-cases. E.g. if the data received from a socket is corrupt, or there was an
unhandled exception that you as a programmer did not have in your mental model
while writing the code. These events should be logged at the `Error` level.

At this level human beings are normally directly alerted.

`Warn` are for things like 100 failed password attempts within 5 minutes, for
one of your users, or a temporary network glitch while communicating with a
"resource" such as your database.

If these events for an anomaly and persist over time, humans may be alerted.

At `Info` level, I like to put events and gauges that measure company- relevant
stuff, like when users sign in, sign up, an integration has to perform a retry
or a service was started/restarted.

`Debug` level is the default level and the work-horse. You normally log all
metrics at this level.

`Verbose` is the level when you want that little extra. Not normally enabled.

### Field and Fields

Message fields may be interpolated (injected) into the template string of an
`Event`. The word "template" is used, because the template string should not
vary between requests/users, but be a 'static' string, which can be hashed and
used for grouping in your logging infrastructure.

When reading legacy code, you'll often find code like:

``` fsharp
logger.LogInfo("User {0} logged in", user.name)
```

In Logary, it could look like this:

``` fsharp
Message.event Info "User logged in"
|> Message.setField "user" user.name
|> Message.setFieldFromObject "picture" user.bitmap
|> Logger.logSimple logger
```

Note how the event's template string is a compile time constant, but a field
representing the user's name is added to the message.

By doing it this way, we can be sure that the structured log data remains
structured.

The second function `setFieldFromObject` is used when the compiler complains
that `setField` finds no available overloads.

### Logging from modules

Let's say you have a module in your F# code that you want to log from. You
can either get a logger like shown in *Hello World*, or you can do something
like this:

``` fsharp
module MyCompany.Sub.MyModule

open Logary

let logger = Logging.getCurrentLogger ()

let logInUser () =
  // do something
  Message.event Info "User logged in" |> Logger.logSimple logger
  // do more stuff
```

If you want to name your logger with a specific name, you can use
`Logging.getLoggerByName` instead.

### Logging from a class

Similarly, sometimes you want to log from a class, and perhaps log some metrics too.

```fsharp
namespace MyCompany.Sub

open Logary

type Worker() =
  let logger =
    Logging.getLoggerByName "MyCompany.Sub.Worker"

  let workAmount =
    PointName [| "MyCompany"; "Sub"; "Worker"; "workDone" |]

  let getAnswers (amount : float) =
    // work work work
    42 * amount

  member x.Work (amount : float) =
    // Initially, log how much work is to be done
    // the only "supported" metric is a gauge (a value at an instant)
    // and a derived metric (something you've computed from gauges)
    Message.gauge workName (Float amount) |> Logger.logSimple logger

    // do some work, logging how long it takes:
    let everything = Logger.time logger (fun () -> getAnswers amount)

    // return result
    everything
```

In this example you learnt how to send arbitrary metrics to Logary (the gauge)
and also how to time how long certain method calls take in your system.

Make it a habit to log these sort of gauges all over your code base while
you write your code, to make it much easier to understand the system as it
develops.

In fact, the more you do this, the more use you will have of Logary and of
the dashboard you put up in Kibana (via Logstash) or Grafana (via InfluxDb).
Put it up on a big TV in your office and you'll develop a second sense of
whether the system is doing well or not, just from looking at the graphs.

### Logging fields & templating

Logary supports templating through
[FsMessageTemplates](https://github.com/messagetemplates/messagetemplates-fsharp).
All you have to do is write your templates like:

```fsharp
Message.event "Hi {user}!"
|> Message.setFieldValue "user" "haf"
```

And then targets that support templating will output them 'filled out'.

Message Templates are a superset of standard .NET format strings, so any format
string acceptable to string.Format() will also be correctly processed by
FsMessageTemplates.

 * Property names are written between { and } brackets
 * Brackets can be escaped by doubling them, e.g. {{ will be rendered as {
 * Formats that use numeric property names, like {0} and {1} exclusively, will
   be matched with the Format method's parameters by treating the property names
   as indexes; this is identical to string.Format()'s behaviour
 * If any of the property names are non-numeric, then all property names will be
   matched from left-to-right with the Format method's parameters
 * Property names may be prefixed with an optional operator, @ or $, to control
   how the property is serialised
 * Property names may be suffixed with an optional format, e.g. :000, to control
   how the property is rendered; these format strings behave exactly as their
   counterparts within the string.Format() syntax

### Ticked metrics and gauges – random walk

In the previous section you saw how to create a gauge at a point in your code,
but sometimes you need a metric that runs continuously over time.

This is possible because Logary contains code that can both tick your
metric's computation function at a regular interval, and also has provisions
for sending your metric other metrics, so that you can chain metrics
together.

The `ticker` is where you should return Messages (Gauge or Derived values) and
keep track of how 'far long' you've computed, as to avoid returning the same
messages multiple times.

The `reducer` is what allows your metric to receive values from other metrics,
or from your system-at-large – like the above showcased Gauge logging.

Let's create a metric that just outputs a random walk. Start by opening the
relevant namespaces and modules.

```fsharp
open System // access to Random
open Hopac // access to Job
open Logary // access to the Logary Data Model
open Logary.Metric // access the module functions for metrics
```

Now you can start thinking about what the metric should do and implement the
`ticker : 'state -> 'state * Message list`:

```fsharp
// we'll assume the state is the Random instance an previously outputted
// value:
let ticker (rnd : Random, prevValue) =

  // calculate the next value based on some heuristic or algorithm
  let value =
    let v = (rnd.NextDouble() - 0.5) * 0.3
    if abs v < 0.03 then rnd.NextDouble() - 0.5
    elif v + prevValue < -1. || v + prevValue > 1. then -v + prevValue
    else v + prevValue

  // create a new Message/Gauge metric with this value
  let msg = Message.gauge pn (Float value)

  // return the new state as well as the Messages you want to feed into
  // Logary
  (rnd, value), [ msg ]
```

Remember that you also needed to supply a reducer. In this case, the random
walk metric doesn't have any input from other metrics, so let's just return
the same state as we get in:

```fsharp
let reducer state = function
  | _ ->
    state
```

We also need to create some initial state, so that our metric has someplace
to start computing:

```fsharp
let state =
  let rnd = Random()
  rnd, rnd.NextDouble()
```

Let's write it all up into a Metric which the consuming programmer is
free to name as she pleases:

```fsharp
let randomWalk (pn : PointName) : Job<Metric> =
  Metric.create reducer state ticker
```

Finally, we'll tell Logary about our metric and extend our "Hello World" sample
with shipping metrics into InfluxDb:

```fsharp
// open ... like above
open System.Threading

[<EntryPoint>]
let main argv =
  use mre = new ManualResetEventSlim(false)
  use sub = Console.CancelKeyPress.Subscribe (fun _ -> mre.Set())

  let influxConf =
    InfluxDb.create (InfluxDb.InfluxDbConf.create(Uri "http://192.168.99.100:8086/write", "logary", batchSize = 500us))
                    (PointName.ofSingle "influxdb")

  use logary =
    withLogaryManager "Logary.Examples.MetricsWriter" (
      withTargets [
        Console.create (Console.empty) (PointName.ofSingle "console")
        influxConf
      ]
      >> withMetrics [
        MetricConf.create (Duration.FromMilliseconds 500L) (PointName.ofSingle "henrik") Sample.randomWalk
      ]
      >> withRules [
        Rule.createForTarget (PointName.ofSingle "console")
        Rule.createForTarget (PointName.ofSingle "influxdb")
      ]
      >> withInternalTargets Info [
        Console.create Console.empty (PointName.ofSingle "console")
      ]
      >> run
    )
    |> run

  mre.Wait()
  0
```

Now when run, your metric will feed a random walk into InfluxDb, listening on 192.168.99.100.

### Derived metrics

The above example was self-sufficient, but you sometimes want to create derived metrics
from events or gauges happening inside your own application.

This sample demonstrates how to create a derived metric from other simpler
ones. It generates an exponentially weighted moving average from
login gauges. The login gauges are sent one-by-one from the login code.

```fsharp
open Logary
open Logary.Metrics
open Hopac

let loginLoad : Job<Stream<Message>> = job {
  let! counter = Counters.counter (PointName.ofSingle "logins")
  let! ewma = Reservoirs.ewma (PointName.ofSingle "loginsEWMA")
  do! ewma |> Metric.consume (Metric.tap counter)
  return Metric.tapMessages ewma
}
```

By wrapping it up like this, you can drastically reduce the amount of code
a given service sends by pre-computing much of it.

It's also a good sample of reservoir usage; a fancy name of saying that
it's an algorithm which works on more than one gauge at a time, to produce
a derived metric.

**More documentation on derived metrics to follow!** (including how to register
them in Logary).

## Using logary in a library

The above guide serves to explain how you use Logary in a service or
application, but what if you have a library and don't want to take a dependency
on a particular logging framework?

For this use-case Logary provides F# facades that you can reference using paket.
I've created a [sample
library](https://github.com/logary/logary/tree/master/examples/Libryy) for you
to have a look at. Note how `paket.references` specifies `Facade.fs` as a file
dependency. The corresponding `paket.dependencies` contains the entry below.

```
github logary/logary src/Logary.Facade/Facade.fs
```

In my Rakefile I have a small replacement script, which sets the library's
namespace inside the referenced `Facade.fs` file.

```bash
ruby -pi.bak -e \
  "gsub(/namespace Logary.Facade/, 'namespace Libryy.Logging')" \
  paket-files/logary/logary/src/Logary.Facade/Facade.fs
```

Or in FAKE style:

```fsharp
Target "LoggingFile" (fun _ ->
    ReplaceInFiles [ "namespace Logary.Facade", "namespace Kafunk.Logging" ]
                   [ "paket-files/logary/logary/src/Logary.Facade/Facade.fs" ]
)
```

Inside the library you use the logger just like you'd expect:

``` fsharp
module Libryy.Core

// Note: this library has no reference to Logary proper!
open Libryy.Logging

let work (logger : Logger) =
  fun () ->
      Message.event Warn "Hey {user}!"
      |> Message.setFieldValue "user" "haf"
      |> Message.setSingleName "Libryy.Core.work"
      |> Message.setTimestamp 1470047883029045000L
  |> logger.log Warn
  |> Async.RunSynchronously

  42

let simpleWork (logger : Logger) =
  logger.logSimple (Message.event Error "Too simplistic")
  43
```

The service/application which *does* reference the `Logary` nuget, also
references `Logary.Adapters.Facade` and then creates a new Logger specifically
for the library which it aims to ship logs from.

```fsharp
// opens ...
open Logary.Adapters.Facade

// let main ... =

  use logary =
    withLogaryManager "Servizz.Program" (
      withTargets [ Console.create Console.empty (PointName.ofSingle "console") ]
      >> withRules [ Rule.createForTarget (PointName.ofSingle "console") ])
    |> run

  // for the statics:
  LogaryFacadeAdapter.initialise logary

  // if you need a Logger instance:
  let logger = logary.getLogger (PointName.ofSingle "Libryy")
  let res = Libryy.Core.work (LoggerAdapter.createGeneric logger)
```

Outputs:

```
W 2016-08-01T10:38:03.0290450+00:00: Hey haf! [Libryy.Core.work]
  user => "haf"
```
## Rutta

Rutta is software for shipping Messages between computers. Either from your own
services or from Windows Performance Counters. This is useful if you want your
services to ship all logs to a central point, before batching it and sending it
off to InfluxDb. It's also useful if you want to firewall off a single subnet
for certain processing and only have a single point ship logs and metrics.

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

On Windows you do `./rutta.exe -- --pub-to ...` - note the twp extra dashes
before the parameter list. This is to avoid Topshelf munching the arguments
away.

### The Shipper – from environment to Proxy or Router

Enables log shipping from hosts that are not directly connected to the router
nor to InfluxDB.

Should be spawnable on Unix. Should be service-installable on Windows using
TopShelf.

#### Pushing Shippers

Shippers CONNECT PUSH sockets to the Router's PULL socket.
See http://lists.zeromq.org/pipermail/zeromq-dev/2012-February/015917.html

``` bash
./rutta --push-to tcp://headnode:6111
```

During network splits, the sending
[PUSH socket blocks](http://api.zeromq.org/3-2:zmq-socket#toc14).

#### Publishing Shippers

``` bash
./rutta --pub-to tcp://headnode:7111
```

During network splits, the sending XPUSH socket drops messages.

### The Proxy – from Shipper to Router

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

### The Router – from Shipper or Proxy to Target

Implements Fan-In using PULL or SUB of Messages from ZMQ. Forwards internally
to a Target.

V1 only implements the InfluxDB target.

#### Pulling Routers

BINDs a PULL socket on a specified NIC/IP and PORT. Configures a single
internal Target that pushes the received data.

``` bash
./rutta --router tcp://192.168.10.10:7113 \
        --router-target influxdb://user:pass@host:8086/write?db=databaseName
```

During network splits, the listening
[PULL socket blocks](http://api.zeromq.org/3-2:zmq-socket#toc15).

#### Subscribing Routers

BINDs a SUB socket on a specified NIC/IP and POST. Configures a single internal
Target that pushes the received data.

``` bash
./rutta --router-sub tcp://192.168.10.10:7113 \
        --router-target influxdb://user:pass@host:8086/write?db=databaseName
```

**Serialisation** for Rutta is done using
[FsPickler](https://nessos.github.io/FsPickler/tutorial.html#Picklers-and-Pickler-combinators).
Since FsPickler uses a binary format, it should be assume to break for any given
minor upgrade of FsPickler.

Each ZMQ message contains a Message (see DataModel.fs) in the binary form
given by the serialiser chosen.

## Target Maintainers Wanted!

Are you interested in maintaining a target? Let [me know](mailto:henrik@haf.se)
or file a PR demonstrating your work.

## Building

Assuming you have Ruby 1.9.3 or later installed:

``` bash
git clone --recursive -j8 git://github.com/logary/logary.git
cd logary
bundle
bundle exec rake
```

### Building a signed version

``` bash
# first place your files here:
# tools/logary.pvk
# tools/logary.pvk.password
# tools/logary.spc
LOGARY_SIGN_ASSEMBLY=true bundle exec rake
# DEBUG=true LOGARY_SIGN_ASSEMBLY=true bundle exec rake
```

## Contributing

Clone it like above. Ensure you can build it. Open `Logary.sln`. 
Make a change, send a PR towards master.

### Writing a new target

 1. Create a new .net 4.5 class library in F#, under `target` and add that to Logary.sln.
 1. Copy the code from Logary's Target_Noop.fs, which contains the basic structure.
    There are more docs in this file.

## License

[Apache 2.0][apache]

 [apache]: https://www.apache.org/licenses/LICENSE-2.0.html
