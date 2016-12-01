(**
- title: Logging and metrics in .Net
- description: An introduction to logging, metrics and health checks on .Net
- author: Henrik Feldt
- theme: league
- transition : default

***

### What is Logary

A high-performance .Net library for dealing with

- semantic logging of **events**
- **metrics** collection and generation

<p>
  <img src="https://raw.githubusercontent.com/logary/logary-assets/master/graphics/LogaryLogoSquare.png" width="200" />
</p>

Apache 2.0 licensed & open source. [@logarylib][twitter-logarylib]

---

## Who has made Logary

 - Henrik Feldt [@haf][github-haf]
 - Community [contributors][github-contributors]

***

## How is the project run?

In the spirit of open source.

 - First version released in 2013
 - Stable and tested.
 - Continuously integrated and accepting pull requests.

### Installing

Easy! In `paket.dependencies`:

```
source https://www.nuget.org/api/v2
nuget Logary
```

***

### Hello World – Starting

<div style="display: none">
*)
#r "Hopac.dll"
#r "Hopac.Core.dll"
#r "Logary.dll"
#r "Logary.Metrics.WinPerfCounters.dll"
open Hopac
open Logary
open Logary.Targets
open Logary.Configuration
(**
</div>
*)
let logger = Logging.getLoggerByName "Sample"

let logary =
  withLogaryManager "intro" (
    withTargets [ LiterateConsole.create (LiterateConsole.empty) "console" ]
    >> withRules [ Rule.createForTarget "console" ])
  |> run

(**

***

## Hello World – Events

*)
open Message
logger.info (eventX "Hello {world}!" >> setField "world" "Earth")
(**

---

## Hello World – Gauges & Timers

You can also work directly with the `Message` module.

*)
let res, msg =
  let dp = PointName.parse "Sample.fun" in
  Message.time dp (fun () -> 66) ()
(**
Yields `msg` and `res`:
*)
(*** include-value: msg ***)
(*** include-value: res ***)
(**

This creates a [Gauge][readme-gauge] value.

---

## Hello World – Gauges & Timers (2)

 - `name` is the *location* or *source*
 - `value` contains the `Gauge` value, a discriminated union
 - `timestamp` is in nanoseconds since Unix epoch
 - `fields` and `context` can be filled out by *middleware* or simply attached
   to the `Message` value
 - `level` specifies how urgent the gauge is – useful in *health checks*

---


## Hello World – Gauges (Counter)

*)
let dp = PointName.parse "MyApp.Requests"
Message.gaugeWithUnit dp Scalar (Int64 1L) |> logger.logSimple
(**

---

## Hello World – Derived Metrics (Meters & Histograms)

*)
open Logary.Metrics

let loginsPerSecond : Job<Stream<Message>> = job {
  let! counter = Counters.counter (PointName.ofSingle "logins")
  let! ewma = Reservoirs.ewma (PointName.ofSingle "logins")
  do! ewma |> Metric.consume (Metric.tap counter)
  return Metric.tapMessages ewma
}
(**

***

## Hello World – C# – Events

    [lang=csharp]
    logger.LogEvent(LogLevel.Debug, "Hello {world}!", new {
        world = "Earth"
    });

---

## Hello World – C# – Gauges & Timers

    [lang=csharp]
    logger.TimePath("Sample.fun", () => 66);

***

## Gauges & Metrics

Supports polling.

<div style="display: none">
*)
open Logary.Metric
open Logary.Metrics
open Logary.Metrics.WinPerfCounter
(**
</div>

---

## Gauges & Metrics (2)

Custom metric: a random walk
*)
open System
let create pn : Job<Metric> =
  let reducer state = function _ -> state
  let ticker (rnd : Random, prevValue) =
    let value =
      let v = (rnd.NextDouble() - 0.5) * 0.3
      if abs v < 0.03 then rnd.NextDouble() - 0.5
      elif v + prevValue < -1. || v + prevValue > 1. then -v + prevValue
      else v + prevValue
    (rnd, value), [| Message.gaugeWithUnit pn Seconds (Float value) |]
  let state = let rnd = Random() in rnd, rnd.NextDouble()
  Metric.create reducer state ticker
(**

***

## Targets – Core

Transform `Message` values into target-specific representations.

<style type="text/css">
ul.boxes {
  margin: 0;
  padding: 0;
  list-style: none;
}
ul.boxes li {
  outline: 2px dashed #eee;
  padding: 3vh;
  margin: 1vh;
  float: left;
}
</style>

<ul class="boxes">
 <li>DB</li>
 <li>ElasticSearch</li>
 <li>Heka</li>
 <li>InfluxDb</li>
 <li>Logstash</li>
 <li>Mailgun</li>
 <li>RabbitMQ</li>
 <li>Riemann</li>
 <li>Shipper</li>
 <li>Zipkin</li>
 <li>...</li>
</ul>

---

## Targets – Commercial

<ul class="boxes">
 <li>Mixpanel – event analytics; make your site behave smarter</li>
 <li>OpsGenie – alert on error conditions</li>
 <li>SumoLogic – logging and metrics as a SaaS</li>
</ul>

---

## Targets – Surface area

<div style="display: none">
*)
open Hopac
open Hopac.Infixes
open Logary
open Logary.Target
open Logary.Internals
(**
</div>
*)
let loop  (runtime  : RuntimeInfo)
          (requests : RingBuffer<TargetMessage>)
          (shutdown : Ch<_>)
          (saveWill : obj -> Job<unit>)
          (lastWill : obj option) =
  let rec loop () : Job<unit> =
    Alt.choose [
      shutdown ^=> fun ack ->
        ack *<= () :> Job<_>
      RingBuffer.take requests ^=> function
        | Log (message, ack) ->
          // Here's where the majority of work is done!
          job { do! ack *<= ()
                return! loop () }
        | Flush (ackCh, nack) ->
          job { do! Ch.give ackCh () <|> nack
                return! loop () }
    ] :> Job<_>
loop ()
(**

---

## Targets – Why?

 - Allows for **async network communication**
 - Explicit **flush** can be used to ensure 'baseline' between all targets
 - Callers can **NACK/abort flush** that take too long
 - Callers can **wait for ACKs**

---

## Targets – Why? (2)

 - Supports back-pressure through its RingBuffers
 - Supports **stashing state** with supervisor on exceptions
 - **Let-it-crash**. The hidden **supervisor** will restart => stable software

***

## The Facade

The [Logary Facade][readme-facade] lets you log in a structured manner from
libraries, without taking a dependency on Logary. That way you reduce churn.

    [lang=fsharp]
    open Libryy.Logging

    let coreLogger = Log.create "Libryy.Core"

    let work (logger : Logger) =
      logger.logWithAck Warn (
        Message.eventX "Hey {user}!"
        >> Message.setFieldValue "user" "haf"
        >> Message.setSingleName "Libryy.Core.work"
        >> Message.setTimestamp 1470047883029045000L)
      |> Async.RunSynchronously
      42

***

## Adapters

Get logs from libraries.

 - F# [Facade adapter][readme-facadeadapter]
 - [EventStore][readme-eventstore]
 - [FsSql][readme-fssql]
 - [Suave][readme-suave]
 - [Topshelf][readme-topshelf]
 - ...

---

## Adapters – F# facade

Lets you extract the logs from any library using the [Logary
Facade][readme-facade] without writing any code for it.

It will automatically translate logged data into Logary's
[DataModel][readme-datamodel].

***

## Services – Rutta

Lets you **ship** *from* nodes in a subnet, to a **proxy** that can forward to a
**router** that knows about your targets.

---

## Services – Rutta (2)

<img title="Rutta architecture" src="images/rutta.png" width="700" />

---

## Services – SQL Server Health

A service that runs on heavily loaded SQL Server instances to ship their metrics

---

## Services – Suave Reporter

A service you can use for logging over HTTP. API is isomorphic to that of
`Message`, built using [Suave.io][suave-io] to be scalable and light-weight.

---

## Services – SuaveReporter (2)

Use together with [logary-js][github-logaryjs]

<img title="Logary JS at NPM" src="images/logary-js-screen.png" />

***

## In summary

 - A one-stop-shop for logging and metrics
 - Heavily [documented][github-logary]
 - Community and commercial support available
 - Constructed using the lessons learned from building distributed systems

 [readme-gauge]: https://github.com/logary/logary#pointvaluegauge
 [readme-facade]: https://github.com/logary/logary#using-logary-in-a-library
 [readme-facadeadapter]: https://github.com/logary/logary#the-logary-facade-adapter
 [readme-datamodel]: https://github.com/logary/logary#tutorial-and-data-model
 [readme-eventstore]: https://github.com/logary/logary#evenstore-adapter
 [readme-fssql]: https://github.com/logary/logary#fssql-adapter
 [readme-suave]: https://github.com/logary/logary#suave-adapter
 [readme-topshelf]: https://github.com/logary/logary#topshelf-adapter
 [github-logary]: https://github.com/logary/logary#logary-v4
 [github-haf]: https://github.com/haf
 [github-contributors]: https://github.com/logary/logary/graphs/contributors
 [github-logaryjs]: https://github.com/logary/logary-js#logary-js
 [twitter-logarylib]: https://twitter.com/logarylib
 [suave-io]: https://suave.io
*)
