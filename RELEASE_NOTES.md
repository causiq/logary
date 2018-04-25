#### 5.0.0-beta.4
* Support parsing exn type and inner exception messages from .Net and Java stacktraces. Thanks @haf
* Fix most of #294, thanks @haf
* Fix #285 for LiterateConsole. This should be it. Thanks @haf.
* Move colourising tokenisers to a module more easily discoverable. Thanks @haf
* Fix #300 naming of Events.miniLevel -> minLevel, thanks @haf
* Fix #313 - passing along exns in the right format, thanks @haf
* Fix #314 - add a few missing SI units

#### 5.0.0-beta.3
* Targets: AliYun target ported to .Net Core
* Targets: StackDriver target goes via JSON encoding; makes it total (AFAIK)
* Codecs: log4j XML fix correcting its timestamp handling
* Ingestion: Publish HTTP ingestion as package
* Ingestion: Publish UDP ingestion as package

#### 5.0.0-beta.2
* Rutta: support subcommand specification via App.config
* Rutta: better logging about what targets start in Router
* Core: Can generate nugets

#### 5.0.0-beta
* Core: Fully support both .Net Core and .Net FW, thanks @haf
* Core: Completely new internals with very similar public API, thanks @lust4life, @haf
* Core: Logary is now a simple stream processing library with a `processing` step, responsible for filtering, sorting, grouping, munging and hugging `Message` values as they flow past, and finally choosing what `target` they end up in. Thanks to @haf for starting and @lust4life for completing this large piece of functionality. This also means the `Rule` API has changed.
* Core: Message.value is now a string, use the `addGauge` family of functions to add gauges to it
* Core: Move `RuntimeInfo` into `TargetAPI` interface, thanks @haf
* Core: Map`2 -> HashMap`2, thanks to @mrange for the data structure—see https://github.com/mrange/fsharpadvent2016 for details. We expect this to lead to fewer allocations and a faster library. Thanks @haf for building it into Logary. (see `Logary.Message`)
* Core: `Message.fields` -> `Message.{context,setField,tryGetField}`. You can also use `Message.Patterns` to pattern-match over all possible types of values that can be in the context, when iterating over them. Thanks @lust4life, @haf
* Core: Use `TypeShape` instead of arbitrary reflection to destructure objects, thanks @lust4life, @haf and @eiriktsarpalis for creating the library in the first place. (see `Logary.Internals.TypeShape.{Core,Utils}`)
* Core: Uses @neoeinstein's Chiron 7 from https://github.com/neoeinstein/chiron/tree/chiron-7 for all JSON-oriented tasks. Thanks @lust4life for getting this started. (see `Logary.Internals.Chiron`)
* Core: Upgraded Aether (under `Logary.Internals.Aether`)
* Core: Console target now writes single-lines only. Thanks @lust4life, @haf
* Core: Literate console target is much better off with improvements across the board, thanks @adamchester
* Core: Logary targets are now restarted with a Supervisor, which doubles as a generic Hopac-job/alt supervisor. Thanks to @neoeinstein for the initial implementation, to @lust4life for integrating it and to @haf for testing and stabilising and bugfixing it.
* Core: Logary targets now have access to a `Will` abstraction, should they choose to crash. The will will be sent back to the target on reinstantiation. Thanks @haf
* Core: Logary targets can now specify a `Policy` of how they should be restarted on failure. By default (`createSimple` used), they are restarted with exponential backoff, forever.
* Core: How `Rule` works has been changed, see https://github.com/logary/logary/issues/274#issuecomment-349201804
* Core: Logary is now dual licensed to promote real open source projects and contributors to them. See https://github.com/logary/logary/blob/master/LICENSE.md
* Core: Added license-check on start
* Core: FsFormatting and JSON formatting now does cyclic reference tracking.
* Core: Added *Codec* concept, which takes either a string or a chunk of a byte array and converts it to a Message. Thanks @haf and to @tradera for sponsoring this change.
* Core: Added these codecs: `plain` (straight to `Message.value`), `json` (destructures to Message with a best-effort analysis of the contents), `binary` (for FsPickler based inputs) and `log4jXML` (from https://github.com/apache/logging-log4net/blob/master/src/Layout/XmlLayoutSchemaLog4j.cs appenders with https://logging.apache.org/log4net/release/howto/chainsaw.html), thanks @haf Also see the *Rutta* section below for details about these.
* Config: Support for custom JSON formatters, thanks @lust4life, @haf
* Config: Support for custom Literate destructurers, projectors, thanks @lust4life
* Config: Support for URI-based configuration of targets, see `Logary.Configuration.Uri`, thanks @haf
* Metrics: New method for tick-based metrics—see examples. Thanks @lust4life
* Metrics: Added methods for timing Jobs and Alts, thanks @haf
* Metrics: Support pretty-printing gauges like `2.3 days` or `1.616 µs`, thanks @haf
* Metrics: The new Processing feature (@lust4life) together with `Reservoirs` and `Snapshot` values (@haf) lets you create histograms in Logary, on the fly.
* Facade: Bump to v3 with Gauge containing `float`.
* Build: Move to FAKE, remove Albacore, thanks @haf
* Build: Run all targets' tests as part of build, thanks @haf
* Docs: README has been mostly updated to the new API, thanks @lust4life, @haf
* Docs: README now has support for letting Logary remember the last N messages and batch-sending all remembered message to M targets upon a trigger, like getting an `Error` message.
* Docs: Samples have been updated to the new API, thanks @lust4life, @haf
* Targets: The StackDriver target has been rewritten, bumped to latest versions of the SDK and has gotten batching added. Thanks to @tradera for sponsoring this change and @haf for implementing it.
* Targets: The InfluxDB target has been rewritten and has a leaner and meaner mapping from the new `Message` structure of Logary into tags (from context values, Logary tags) and fields (from gauges, and fields). It will now default to the `value={..}` syntax if you log a single gauge, with the logger + gauge as the measurement name. If you log multiple gauges, each gauge is put as a separate *field value* and the *measurement* is your logger's name. Furthermore, gauges are also formatted when sent. For the full specification, please see https://github.com/logary/logary/blob/master/src/targets/Logary.Targets.InfluxDb/Targets_InfluxDb.fs#L143 (under `nameGauges`), thanks @haf.
* Targets: All targets have been upgraded to their latest versions of their respective libraries. This includes HTTP.fs—HTTP-based targets have been updated to use the compositional/filter-based approach to composing HTTP requests. Thanks @haf See https://github.com/logary/logary/blob/master/src/targets/Logary.Targets.OpsGenie/Targets_OpsGenie.fs#L277 for an example
* Targets: I (@haf/@logibit) have open sourced the Mixpanel and OpsGenie targets and updated their implementations to be even better, including batch-processing of messages: however, they are still licenced commercially no matter where they are running. Be honest now, dear user!
* Rutta: Added Argu and commands for easier usage and AppSettings-based configuration, thanks @haf, @tradera
* Rutta: Fix TopShelf API to be style-ideomatic, thanks @haf, @tradera
* Rutta: Added UDP ingestion, thanks @haf, @tradera
* Rutta: Added HTTP ingestion, thanks @haf, @tradera
* Rutta: Added ZMQ STREAM/TCP ingestion, thanks @haf, @tradera
* Rutta: Added support for spawning multiple listeners, thanks @haf, @tradera
* Rutta: Added support for specifying a codec for each listener. All of the above can now look like this to spawn a few different listeners, codecs and ingestion methods, together with the URI-based config improvement: `rutta.exe router  --listener tcp 127.0.0.1:20001 json --listener tcp 127.0.0.1:20004 log4jxml --listener udp 127.0.0.1:20005 plain --target console://./ --target stackdriver://google/?projectId=my-project --verbose` In effect, this means you can run Rutta as a side-car to e.g. a docker container, in e.g. UDP mode, thereby increasing stability and letting non-.Net services, or C# services log into Logary and its targets. In the above case, we can configure the UDP output appender from log4net and have Rutta ingest that and either: send it to a target (the router command), proxy to another rutta instance (the proxy command) or poll system metrics or other persistent gauges where it is running (the shipper command). Thanks to @haf for coding and @tradera for sponsoring.
* C# API and C# Facade: Carries over from v4. Thanks to Lynx Asset Management for sponsoring it.
