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

Sponsored by
[qvitoo – A.I. bookkeeping and Logary in production since many years](https://qvitoo.com/?utm_source=github&utm_campaign=logary).

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

 - Logary – the main package and logging and metrics library.
 - Logary.CSharp - C# facade which makes it more 'object oriented'.
 - Logary.Facade - a single file you can import into your own F# library.
 - Adapters (things that you inject into libraries to send to Logary):
   * Logary.Adapters.CommonLogging – so that CommonLogging can log to Logary.
   * Logary.Adapters.EventStore – so that [EventStore](https://geteventstore.com/) can ship its logs via Logary.
   * Logary.Adapters.Facade – an adapter for the above; works for arbitrary namespace.
   * Logary.Adapters.FsSql – an adapter for the [FsSql](https://www.nuget.org/packages/FsSql/) library.
   * Logary.Adapters.Hawk - an adapter for Logibit's [Hawk](https://www.nuget.org/packages/Hawk/) library.
   * Logary.Adapters.log4net – an adapter for log4net to log into Logary.
   * Logary.Adapters.Suave – an adapter for Suave to log into Logary.
   * Logary.Adapters.Topshelf – an adapter for Topshelf to log into Logary.
 - Metrics (things that talk to the environment to get data):
   * Logary.Metrics.WinPerfCounters – an API to access Windows Performance Counters.
 - Targets (things that write messages/events/metrics to something):
   * Logary.Targets.DB – target for writing logs into an arbitrary database.
     (SQL Server, MySQL, PostgreSQL, sqlite and so on...)
   * Logary.Targets.DB.Migrations – uses [FluentMigrator](https://github.com/schambers/fluentmigrator/)
     to create and then upgrade your DB between versions of Logary.
   * Logary.Targets.Heka – target for writing events and metrics to Heka.
   * Logary.Targets.InfluxDb – target for writing metrics and annotations (from events) to InfluxDb.
   * Logary.Targets.Logstash – target for writing events and events to Logstash over ZeroMQ.
   * Logary.Targets.Mailgun – target for e-mailing yourself warnings, errors and fatal errors.
   * Logary.Targets.Riemann – target for sending events and metrics to Riemann
   * Logary.Targets.Shipper – a target that sends events and metrics to the Router or Proxy (see services)
 - Services (things that run as services on their own):
   * Logary.Services.Rutta – a service that either:
     - Ships Windows Performance Counters to the `Router` or `Proxy`, pushing via a PUB or PUSH ZeroMQ socket
     - Proxies `Message`s between the `Shipper` and the `Router`, listening on a ZeroMQ XSUB/XPUB socket
     - Routes `Message`s to Targets, listening on a ZeroMQ SUB or PULL socket
     - Note that the shipping feature is its own target as well. Why? So that you can send logs in an efficient,
       high-performance manner between machines, without going through a potentially destructure
       mapping to another serialisation format or through another log router (Heka, Logstash) which
       also may change your data structure.
   * Logary.Services.SQLServerHealth – an unmaintained service that keep track of performance for highly loaded SQL Servers
   * Logary.Services.SuaveReporter – a well-maintained Suave WebPart that you run as a part of your Suave
     server, which enables you to use [logary-js](https://www.npmjs.com/package/logary-js).

## Data Model

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

### Hierarchical logging

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

Clone it like above. Ensure you can build it. Open `v4.sln`. 
Make a change, send a PR towards master.

## License

[Apache 2.0][apache]

 [apache]: https://www.apache.org/licenses/LICENSE-2.0.html
