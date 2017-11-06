- remove pollServices in registry

  Because supervisor will supervise minions, if an exception occurs, it will be handled according to policy. So there's no need for state polling, because we can't do much if we can get the status. the user's handle logic can be expressed by policy.

- remove healthcheck & metric

  >  The aim here is to normalise the Gauge/Derived/Message semantics into a single semantic of 'things computed from events from your app'.

  - remove healthcheck

    Looks like metric can also be seen as a healthcheck, such as windows performance counters. when outside tick , metric generate some msg, which will be processed by user defined engine , they can decide their meaning (log or metric or healthcheck...) there.

  - remove metric

    metric can be implemented by engine's process expression builder, what we need to do is schedule jobs for tick, in the corresponding time(like old metricConf.tickInterval) or manually tick. Metric's data sources can have two types, one is similar to WPC, generate data in real-time when ticked .the other is generate through the normal log message.

- support multi targets on internallogger and apply rules when send msg to target
- remove loglevel on Logger interface

  message has the log level info, targets have the rules info, before putting msg to target, apply rules on message to decide if really send to target.

- redefined Message

  The aim here is to normalise the Gauge/Derived/Message semantics into a single semantic of 'things computed from events from your app'.

  - remove FieldModule.fs
  - use FsMessageTemplate for default MessageWriter
  - json support will use fspickler.json

- todo

  - more unit test
  - reorganize types in Events.fs
  - chiron new version in Serialisation.fs  # no need use fspickler instead
  - failwith "todo"
  - maybe obsolete
    - PromisedLogger.fs

      create logger is no longer async, just a function with registry instance

    - service.fs

      > because it has an abstraction Service around each service and that abstraction isn't crisp. I'm considering adding a behaviour tree which is a more natural polling mechanism with built-in compensatory logic.

    - Metrics.Ticked.fs  ->  replace by Events.Ticker
    - Transformers.fs -> replace by Events.xxx (should implement by events pipe style)
