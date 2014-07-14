namespace Logary.Configuration

/// A module the ties together Logstash, Riemann, Console/Debug, Graphite and
/// some health checks into a nicely configured Logary instance.
///
/// TODO: graphite, riemann, health checks
[<AutoOpen>]
module TargetDefaults =

  open Logary
  open Logary.Target
  open Logary.Rule
  open Logary.Configuration

  /// Run with console and debugger targets with sane configurations.
  [<CompiledName "GoodDefaults">]
  let goodDefaults name =
    // TODO: should include Riemann health checks
    // TODO: should include a Graphite target
    confLogary name
    |> withTargets
      [ Console.create Console.ConsoleConf.Default "console"
        Debugger.create Debugger.DebuggerConf.Default "debugger" ]
    |> withRules
      [ { Rule.forAny "console" with level = Debug }
        { Rule.forAny "debugger" with level = Debug } ]

  /// Run with console and debugger targets with sane configurations as well
  /// as a logstash configuration.
  [<CompiledName "GoodDefaultsAndLogstash">]
  let goodDefaultsAndLogstash name hostname port =
    goodDefaults name
    |> withTarget (Logstash.create (Logstash.LogstashConf.Create(hostname, port)) "logstash")
    |> withRule ({ Rule.forAny "logstash" with level = Debug })

  /// Start logary with sane SOA/service-defaults, remember to call
  /// shutdownLogary at the end of the program. Pass the name of the
  /// service you are configuring.
  [<CompiledName "RunWithGoodDefaults">]
  let runWithGoodDefaults name =
    goodDefaults name
    |> validateLogary
    |> runLogary

  /// Run with console and debugger targets with sane configurations as well
  /// as a logstash configuration, and start logary.
  [<CompiledName "RunWithGoodDefaultsAndLogstash">]
  let runWithGoodDefaultsAndLogstash name hostname port =
    goodDefaultsAndLogstash name hostname port
    |> validateLogary
    |> runLogary
