# Logary v4

Follow Logary at twitter: [@logarylib](https://twitter.com/logarylib)

Chat and support and get support:
[![Gitter chat](https://badges.gitter.im/logary.png)](https://gitter.im/logary/logary)

## Why?

Logary is a high-performance, semantic logging, health and metrics library for
.Net.

 - Full support for Semantic Logging
 - F# idiomatic code
 - Using C#? Then `Logary.CSharp` is for you!
 - Looking for an F# alternative to [`LibLog`](https://github.com/damianh/LibLog)?
   Jump to [`Logary.Facade`](#using-logary-in-a-library).
 - Never throws exceptions
 - Low overhead logging – evaluate your Message only if a level is switched on
 - Supports [Hierarchical logging](#rule--hierarchical-logging)
 - Add metrics to your service/app!
 - A wide range of adapters and targets to choose from!

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

## Table of Contents

  * [Logary v4](#logary-v4)
    * [Why?](#why)
    * [Install it](#install-it)
    * [Table of Contents](#table-of-contents)
    * [Hello World](#hello-world)
    * [Overview](#overview)
    * [Tutorial and Data Model](#tutorial-and-data-model)
      * [PointName](#pointname)
      * [PointValue\.Event](#pointvalueevent)
      * [PointValue\.Gauge](#pointvaluegauge)
      * [PointValue\.Derived](#pointvaluederived)
      * [Rule &amp; Hierarchical logging](#rule--hierarchical-logging)
      * [Log Level](#log-level)
      * [Field and Fields](#field-and-fields)
      * [Logging from modules](#logging-from-modules)
      * [Logging from a class](#logging-from-a-class)
      * [Logging fields &amp; templating](#logging-fields--templating)
      * [Ticked metrics and gauges – random walk](#ticked-metrics-and-gauges--random-walk)
      * [Derived metrics](#derived-metrics)
    * [Using logary in a library](#using-logary-in-a-library)
      * [How do the error and log methods differ?](#how-do-the-error-and-log-methods-differ)
      * [Passing more information](#passing-more-information)
      * [A note on the FSI](#a-note-on-the-fsi)
      * [More reading](#more-reading)
    * [RabbitMQ Target](#rabbitmq-target)
      * [Usage](#usage)
      * [From NLog\.RabbitMQ, log4net\.RabbitMQ?](#from-nlograbbitmq-log4netrabbitmq)
    * [Comparison to NLog and log4net](#comparison-to-nlog-and-log4net)
    * [Rutta](#rutta)
      * [The Shipper – from environment to Proxy or Router](#the-shipper--from-environment-to-proxy-or-router)
        * [Pushing Shippers](#pushing-shippers)
        * [Publishing Shippers](#publishing-shippers)
      * [The Proxy – from Shipper to Router](#the-proxy--from-shipper-to-router)
      * [The Router – from Shipper or Proxy to Target](#the-router--from-shipper-or-proxy-to-target)
        * [Pulling Routers](#pulling-routers)
        * [Subscribing Routers](#subscribing-routers)
    * [Target Maintainers Wanted\!](#target-maintainers-wanted)
    * [Building](#building)
      * [Building a signed version](#building-a-signed-version)
    * [Contributing](#contributing)
      * [Writing a new target](#writing-a-new-target)
    * [Commercial Targets](#commercial-targets)
      * [Mixpanel](#mixpanel)
        * [Features](#features)
      * [elmah\.io](#elmahio)
      * [Want your SaaS\-logging service as a Target?](#want-your-saas-logging-service-as-a-target)
    * [License](#license)

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
        Console.create (Console.empty) "console"
      ] >>
      // continuously log CPU stats
      withMetrics [
        MetricConf.create (Duration.FromMilliseconds 500L) "cpu" Sample.cpuTime
      ] >>
      // "link" or "enable" the loggers to send everything to the configured target
      withRules [
        Rule.createForTarget "console"
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

## Overview

Logary is itself a library for metrics and events with extensible inputs, *adapters*, and
outputs, *targets*. Further, its *services* run as their own processes or in
[Suave](https://suave.io/?utm_source=github&utm_campaign=logary).

 - **Logary** – the main logging and metrics library. Your app depends on this.
 - Logary.CSharp - C# facade that makes it more *object oriented*.
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
       high-performance manner between machines, without going through a potentially destructive
       mapping to another serialisation format or through another log router (Heka, Logstash) which
       also may change your data structure.
     </blockquote>
   * SQLServerHealth – a service that keeps track of IO/latency performance for highly loaded SQL Servers
   * SuaveReporter – a well-maintained Suave WebPart that you run as a part of your Suave
     server, that enables you to use [logary-js](https://www.npmjs.com/package/logary-js).

## Tutorial and Data Model

The core type is **`Message`**, which is the smallest unit you can log. It has
three kinds of point values: `Event`, `Gauge` and `Derived`. An event is
normally a single line of code and carries a template string. E.g. "User
logged in" is an event's template string, and the `Message` would have a field
"user" => "haf".

### PointName

A point is a location where you send a metric or event from. Usually a module;
in mature projects you also often have the name of the function that you log
from as a part of the point name.

### PointValue.Event

What you expect: `"User logged in"` with a field `"userName"`, `"haf"`.

### PointValue.Gauge

An instantaneous value. Imagine the needle showing the speed your car is going
or a digital display showing the same instantaneous metric value of your car's
speed.

An event is the most simple gauge of value 1.

### PointValue.Derived

A derived value from one or many gauges.

### Rule & Hierarchical logging

It means that you can have one `Rule`/`Logger` at level `Info` for namespace
`MyCompany` and another `Rule` that matches loggers at `MyCompany.Submodule`
which allows Messages of level `Debug` to go through.

A normal use-case for this is when you want to debug a particular module, by
increasing the verbosity of its output (decreasing its log level).

Rules are 'optimistic' by default in that if at least one (or more) rules match a given `Message`,
the most "open" will decide if it gets logged. So if you have two rules:

```fsharp
withRules [
  Rule.createForTarget "console" Info
  Rule.createForTarget "console" Debug
]
```

Then the `Debug` level will "win" and show log output. More generally, a `Rule`
looks like this:

```fsharp
/// A rule specifies what messages a target should accept.
[<CustomEquality; CustomComparison>]
type Rule =
  { /// This is the regular expression that the 'path' must match to be loggable
    hiera         : Regex
    /// This is the name of the target that this rule applies to.
    target        : PointName
    /// This is the level at which the target will accept log lines. It's inclusive, so
    /// anything below won't be accepted.
    level         : LogLevel
    /// This is the accept filter that is before the message is passed to the logger
    /// instance.
    messageFilter : MessageFilter }
```

You can find the configuration in the module with the same name. The
`Rule.empty` value is a null one that accepts all logs from anything.

### Log Level

The highest log level is `Fatal`, which should be reserved for things that make
your service/process crash. Things like; "my disk is full and I'm a database
trying to start", or "I'm a 2-tier service built with a database, that I cannot
do any work without" warrant the `Fatal` level.

At this level human beings are normally directly alerted.

The next level is `Error`, which should be reserved for what you consider to be
edge-cases. E.g. if the data received from a socket is corrupt, or there was an
unhandled exception that you as a programmer did not have in your mental model
while writing the code. These events should be logged at the `Error` level.

At this level human beings are normally directly alerted.

`Warn` is for things like 100 failed password attempts within 5 minutes, for
one of your users, or a temporary network glitch while communicating with a
"resource" such as your database.

If these events for an anomaly persist over time, humans may be alerted.

At `Info` level, I like to put events and gauges that measure company-relevant
stuff, like when users sign in, sign up, an integration has to perform a retry
or a service was started/restarted.

`Debug` level is the default level and the work-horse. You normally log all
metrics at this level.

`Verbose` is the level when you want that little extra. Not normally enabled.

### Field and Fields

Message fields may be interpolated (injected) into the template string of an
`Event`. The word "template" is used, because the template string should not
vary between requests/users, but be a 'static' string, that can be hashed and
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
`Logging.getLoggerByName` instead. (This is different for the Facade file)

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

This enables targets that support templating to output them 'filled out'.

Message Templates are a superset of standard .NET format strings, so any format
string acceptable to string.Format() will also be correctly processed by
FsMessageTemplates.

 * Property names are written between `{` and `}` braces
 * Braces can be escaped by doubling them, e.g. `{{` will be rendered as `{`
 * Formats that use numeric property names, like `{0}` and `{1}` exclusively, will
   be matched with the `Format` method's parameters by treating the property names
   as indexes; this is identical to `string.Format()`'s behaviour
 * If any of the property names are non-numeric, then all property names will be
   matched from left-to-right with the `Format` method's parameters
 * Property names may be prefixed with an optional operator, `@` or `$`, to control
   how the property is serialised
 * Property names may be suffixed with an optional format, e.g. `:000`, to control
   how the property is rendered; these format strings behave exactly as their
   counterparts within the `string.Format()` syntax

### Ticked metrics and gauges – random walk

In the previous section you saw how to create a gauge at a point in your code,
but sometimes you need a metric that runs continuously over time.

This is possible because Logary contains code that can both tick your
metric's computation function at a regular interval, and also has provisions
for sending your metric other metrics, so that you can chain metrics
together.

The `ticker` is where you return Messages (Gauge or Derived values) to
keep track of how 'far along' you've reached, in order to avoid returning the same
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
// we'll assume the state is the Random instance and previously outputted
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

Let's write it all up into a Metric that the consuming programmer is
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

Now when run, your metric will feed a random walk into InfluxDb listening on `192.168.99.100`.

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
it's an algorithm that works on more than one gauge at a time, to produce
a derived metric.

**More documentation on derived metrics to follow!** (including how to register
them in Logary).

## Using logary in a library

The above guide serves to explain how you use Logary in a service or
application, but what if you have a library and don't want to take a dependency
on a specific logging framework, or logging abstraction/indirection library ?

For this use-case, Logary provides F# facades that you can easily reference using Paket.
I've created a [sample
library](https://github.com/logary/logary/tree/master/examples/Libryy) for you
to have a look at. Note how `paket.references` specifies `Facade.fs` as a file
dependency. The corresponding `paket.dependencies` contains the entry below.

```
github logary/logary src/Logary.Facade/Facade.fs
```

In my Rakefile I have a small replacement script that sets the library's
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

Now add to `paket.references` (replace `Logging` with a folder name of your choice, 
or remove to have Paket not place the (single) file in a folder within the project):

```
File: Facade.fs Logging
```

Inside the library you use the logger just like you'd expect:

``` fsharp
module Libryy.Core

// Note: this library has no reference to Logary proper!
open Libryy.Logging
open Libryy.Logging.Message

let work (logger : Logger) =
  logger.warn (
    eventX "Hey {user}!"
    >> setField "user" "haf"
    >> setSingleName "Libryy.Core.work"
    >> setTimestamp 1470047883029045000L)

  42

let simpleWork (logger : Logger) =
  logger.logSimple (Message.event Error "Too simplistic")
  43
```

Or statically:

```fsharp
module Libryy.Core

open Libryy.Logging
open Libryy.Logging.Message

let internal logger = Log.create "Libryy.Core"

let work () =
  logger.info (eventX "Started work")
  48
```

Any service/application that uses `Libryy` *does* reference the `Logary` and `Facade` nugets, e.g.:

```
source https://www.nuget.org/api/v2
nuget Logary
nuget Logary.Adapters.Facade
```

The calling service/application then creates a new `Logger` specifically for the
library that it aims to ship/extract logs from.

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
  LogaryFacadeAdapter.initialise<Libryy.Logging.Logger> logary
  // calls Librry.Logging.Global.initialise ( new logger inst )

  // if you need a Logger instance:
  let logger = logary.getLogger (PointName.ofSingle "Libryy")
  let res = Libryy.Core.work (LoggerAdapter.createGeneric logger)
```

Outputs:

```
W 2016-08-01T10:38:03.0290450+00:00: Hey haf! [Libryy.Core.work]
  user => "haf"
```

By default, the Facade has a global console logger that logs at `Info` level.

The reason for this is that people normally expect output to come in the
'just installed' case, without hunting for `\*.Logging.Global.initialise` first.

### How do the `error` and `log` methods differ?

If you look inside `Facade.fs` you'll find that `LoggerEx` has `error`, `info`,
etc... as extension methods on the `Logger` interface and that these are marked
internal to the library you're working inside.

`error`, `info` and so on are actually message factories that take a `LogLevel`
and return a `Message`. By using them like this `logger.error (eventX "templ")`,
you're only evaluating the constructor for `Message` if and only if the level of
your logger is greater or equal to `error`.

If we were to expand the point-free style (eta-expansion), it would look like
this: `logger.error (fun level -> Message.eventX "templ" level)`, i.e. what you
pass to the `error` extension method is a factory function, and the `Message`
module provides `gauge`, `event` and `eventX` to create the different kinds of
messages.

### Passing more information

Using the event-templates, you can pass more information to be logged:

```fsharp
with ex ->
  logger.error (
    eventX "Unhandled exception for {user}"
    >> setField "user" user.name
    >> addExn ex)
```

Note the placeholder `{user}` for the user's name in the event template. By
default these will be printed to the console, and if you use
`Logary.Adapters.Facade` you may use all the templating features of
[MessageTemplates](https://github.com/messagetemplates/) for plain-text targets.

### A note on the FSI

`Logary.Adapters.Facade`, the adapter for the library Facade, works by
generating a dynamic interface implementation at runtime. It doesn't work very
well if your library is being used from the F# interactive and all the
library's code, including the `Logger` interface is only available in the
interactive state. You'll end up with a `StackOverflowException` if you try
this.

However, the beauty is that when you're in the interactive, you can just let
the library handle logging through the default Facade targets; i.e. you don't
have to initialise Logary proper to use and read logs in the console, from the
Facade.

### More reading

 - [Facade.fs](https://github.com/logary/logary/blob/master/src/Logary.Facade/Facade.fs)
   – the actual file that gets imported into your library.
 - [Facade unit tests](https://github.com/logary/logary/blob/master/src/tests/Logary.Facade.Tests/Facade.fs)
   – the unit tests for the facade file.
 - [Facade Adapter](https://github.com/logary/logary/blob/master/src/adapters/Logary.Adapters.Facade/Logary.Adapters.Facade.fs)
   – the facade adapter (advanced code)
 - [Facade Adapter unit tests](https://github.com/logary/logary/blob/master/src/tests/Logary.Adapters.Facade.Tests/Program.fs)
   – the unit tests for the adapter, which are also good documentation on how to
   use it.

## RabbitMQ Target

I've written a full RabbitMQ target that includes publisher confirms and durable
messaging. It's fully usable from C# too (since C#-ists like RMQ), through the
[builder
API](https://github.com/logary/logary/blob/master/src/targets/Logary.Targets.RabbitMQ/Targets_RabbitMQ.fs#L359).

Docs are [in this
code](https://github.com/logary/logary/blob/master/src/targets/Logary.Targets.RabbitMQ/Targets_RabbitMQ.fs#L29)
– and you'll find the code fairly readable.

### Usage

```fsharp

  let rmqConf =
    { RabbitMQ.empty with
        appId = Some "Logary.ConsoleApp"
        username = "appuser-12345"
        password = "TopSecret1234"
        tls = { RabbitMQ.TlsConf.certPath = "./certs/mycert.pfx"
                RabbitMQ.TlsConf.certPassword = Some "AnotherSecret1243567" }
              |> Some
        compression = RabbitMQ.Compression.GZip
    }

```

Then inside `withTargets`:

```fsharp
RabbitMQ.create rmqConf (PointName.ofSingle "rabbitmq")
```

And the Rule for it:

```fsharp
Rule.createForTarget (PointName.ofSingle "rabbitmq")
```

### From NLog.RabbitMQ, log4net.RabbitMQ?

Here's how you could configure the RabbitMQ target with C#:

```csharp
.Target<Logary.Targets.RabbitMQ.Builder>(
    "rabbitmq",
    conf => conf.Target
        .EnableTls("./cert/path.pfx", "TopSecret12345")
        // many more knobs to tweak if you continue dotting
        .Done()
)
```

Have a look at [this example](https://github.com/logary/logary/tree/master/examples/Logary.CSharpExample) for more details.

## Comparison to NLog and log4net

Why Logary instead of one of the classic logging frameworks?

 - You get semantic logging with Logary
 - More targets to choose from
 - Larger community of target writers
 - Easier to write targets; they can crash and that's handled by Logary internally
 - Support for zero-dependency usage through `Logary.Facade`
 - Better/more extensive `Rule`-based hierarchies
 - Targets can be decoupled from the network and Ack is a first-level primitive
 - You get back an `Alt<Promise<unit>>` that you can use to synchronise your calling code
   for when the log message is required to be durable; you can't do this with NLog or log4net
 - There's an object model you can use from the calling code
 - Logary is F#, so it's easier to keep bug-free relative to many other languages
 - Logary doesn't keep static state around; easy to refactor, easy to extend

## Rutta

Rutta is software for shipping Messages between computers. Either from your own
services or from Windows Performance Counters. This is useful if you want your
services to ship all logs to a central point, before batching it and sending it
off to InfluxDb. It's also useful if you want to firewall off a single subnet
for certain processing and only have a single point ship logs and metrics.

 - v1: Hard-coded supported target types. Initially we'll just support InfluxDB.
 - v2: More configurable target configuration that supports any target.

This service can run in three modes; Shipper, Router and Proxy. Servers can be
implemented using Hopac's lightweight servers. Communication is implemented
using ZMQ and a binary serialisation format.

Bindings look may look like this:

  - `Shipper -> Router`
  - `Shipper -> Proxy`
  - `Proxy -> Proxy`
  - `Proxy -> Router`

[ZMQ socket reference](http://api.zeromq.org/3-2:zmq-socket)

On Windows you do `./rutta.exe -- --pub-to ...` - note the two extra dashes
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

Proxies take inputs from Shippers or other Proxies that publish Messages
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
Since FsPickler uses a binary format, it should be assumed to break for any given
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

Clone it like above. Ensure you can build it. Open `Logary.sln`.  Make a change,
send a PR towards master. To balance the app.config files, try `mono
tools/paket.exe install --redirects --clean-redirects --createnewbindingfiles`

### Writing a new target

 1. Create a new .net 4.5 class library in F#, under `target` and add that to Logary.sln.
 1. Copy the code from Logary's `Target_Noop.fs`, which contains the basic structure.
    There are more docs in this file.

## Commercial Targets

Logary is a production-grade logging and metrics library. We've also built
targets that integrate with external paid services. These are listed here.

### Mixpanel

![Mixpanel screenshot](./tools/mixpanel-screenshot.png "Event processing, analytics on the web")

Learn how people use your app with the world's most advanced mobile & web
analytics.

[Purchase today](mailto:henrik@haf.se?subject=Logary Mixpanel Target)

#### Features

 - Ship logs from your iOS, Android app
 - Ship logs and handle user identification and unique-id tracking from web
 - Use your own domain and server (over HTTPS)
 - Logary listens on your server and forwards your events into Mixpanel
 - Add granular server-side event filtering/enriching/correlation for better
   insights before shipping them onwards.
 - Log web app usage even when Mixpanel is blocked client-side

#### What's included?

We like open source – so in the purchase the reference source is provided so
that it can be debugged like the rest of Logary.

[Send an e-mail to purchase](mailto:henrik@haf.se?subject=Logary%20Mixpanel%20Target)

This assumes you have an account at [Mixpanel](https://mixpanel.com).

### elmah.io

[![elmah screenshot](./tools/elmah.png)](https://elmah.io "Go to elmah.io to see their offering.")

```
source https://www.nuget.org/api/v2
nuget Logary.Targets.Elmah.Io
```

OR:

```
Install-Package Logary.Targets.Elmah.Io
```

#### Usage

Configure *elmah.io* just like you would any normal target.

```fsharp
open Logary
open Logary.Configuration
open Logary.Targets
open Logary.Targets.ElmahIO

withTargets [
  // ...
  ElmahIO.create { logId = "GUID_HERE" } "elmah.io"
] >>
withRules [
 // ...
 Rule.createForTarget "elmah.io"
]
```

Or from C#:

```csharp
// ...
.Target<ElmahIO.Builder>(
  "elmah.io",
  conf => conf.Target.WithLogId("GUID_HERE"))
```

#### What does it look like?

![View from application](./tools/elmah-screenshot-app.png)

You'll get the same view by logging this `Message`:

```fsharp
type Tenant =
  { tenantId : string
    permissions : string }

let exnMsg =
  Message.event Error "Unhandled exception"
  |> Message.setSimpleName "A.B.C"
  |> Message.setFieldFromObject "tenant" { tenantId = "12345"; permissions = "RWX" }
  |> Message.setContextFromMap (Map
    [ "user", box (Map
        [ "name", box "haf"
          "id", box "deadbeef234567"
        ])
    ])
  |> withException Message.addExn
```

This assumes you have an account at [elmah.io](https://elmah.io).

### Want your SaaS-logging service as a Target?

Absolutely! You have two options;

 1. Send a PR with your target that is of equivalent quality as the rest of the
    code-base, including documentation, code-doc, the C# builder API and a
    sample in this file. Then keep that code up-to-date when Logary evolves and
    your SaaS service changes its APIs.
 1. [Send me an e-mail](mailto:henrik@haf.se) and I'll target the target for
    you. Pricing: a small initial fee and then a monthly maintenance fee, you'll
    have a beautiful way of getting logs and metrics to your servers!

    This is by far the easiest option and ensures that your Target is stable and
    easy to use for your customers. I'll even write some Markdown/HTML-formatted
    docs for your site about how to use Logary with your target.

## License

[Apache 2.0][apache]

 [apache]: https://www.apache.org/licenses/LICENSE-2.0.html
