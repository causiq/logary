# Logary

A smart, performant logging system for .Net and mono applications and services.

### Logging Integrations

 - Console logging
 - Debug windows logging
 - TextWriter-based logging
 - Trace logging
 - LogStash logging
 - RestKin/ZipKin logging/tracing
 - NLog integration logging
 - nimrod integration logging (mostly just a log format)
 - MailGun integration, sending e-mails with log data

### Health checks

Logary support defining arbitrary health checks. I suggest you define one for
each resource that your application depends on, allowing you to alert with either
logs or metrics when that resource responds sluggishly or not at all, or is
misbehaving.

### Metrics

 - Graphite/statd metrics sending
 - Riemann metrics sending
 
## Configuring in F#

``` fsharp
let li = 
  confLogary "tests"
  |> validateLogary
  |> runLogary
```

and when you're shutting down gracefully:

```
li |> shutdownLogary // -> Async<Acks>
   |> Async.RunSynchronously
   // TODO: handle whether all targets shut down within the timeout
```

## Configuring in C#

There's a fluent interface for C#, but unline other fluent C# interfaces
like those in MassTransit or NServiceBus, this one doesn't allow your
usage to compile unless you've used it correctly. It makes it simple
to configure Logary for even the most fresh newbie.

```
var x = LogaryFactory.New("Logary Specs",
	with => with.Target<TextWriter.Builder>(
		"console1",
		conf =>
		conf.Target.WriteTo(Console.Out, Console.Error)
			.MinLevel(LogLevel.Verbose)
			.AcceptIf(line => true)
			.SourceMatching(new Regex(".*"))
		));
```

and when you're shutting down gracefully:

```
logManager.Dispose();
```

At this point you might get exceptions if not all targets were shut down
before the configured timeout.

### Loosely based on CodaHale's metrics

 - http://metrics.codahale.com/getting-started/
 - https://github.com/haf/Graphite.NET
 - https://github.com/haf/riemann-health-windows

# Stub Target Outline (e.g. named IPInternal)

```
module Logary.Target.IPInternal 

open FSharp.Actor

open Logary
open Targets

type IPInternalConf =
  { connStr : string }

type internal IPInternalState =
  { a : string }

let requestTraceLoop (conf : IPInternalConf) (svc : ServiceMetadata) =
  (fun (inbox : IActor<_>) ->
    let rec init () = async {
      return! running { a = "" } }

    and running state = async {
      return! shutdown state }

    and shutdown ackChan = async {
      return () }

    init ())


/// Create a new IPInternal target configuration.
let create conf = TargetUtils.stdNamedTarget (requestTraceLoop conf)

/// Create a new IPInternal target configuration.
[<CompiledName("Create")>] 
let CreateC(conf, name)  = create conf name

/// Use with LogaryFactory.New( s => s.Target< HERE >() )
type Builder(conf, callParent : FactoryApi.ParentCallback<Builder>) =

  member x.Done() =
    ! (callParent <| Builder(conf, callParent))

  member x.ConnectionString(conn : string) =
    Builder({ conf with connStr = conn }, callParent)

  new(callParent : FactoryApi.ParentCallback<_>) =
    Builder({ connStr = "" }, callParent)

  interface Logary.Targets.FactoryApi.SpecificTargetConf with
    member x.Build name = create conf name
```

## Aims:

  - Tags
  - Message
  - StackTrace
 
  - Gauges
  - Counters
  - Meters
  - Timers

  - (Histograms)
  - Health Checks
 
  - Riemann target
  - Logstash target over kafka transport
  - Graphite format
