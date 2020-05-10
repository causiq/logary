>
- let pipeLine =
-    Events.events
-    |> Events.tag "queue"
-    |> Pipe.map (fun msg -> {msg with value = "https://github.com/alexandrnikitin/MPMCQueue.NET"})
-
-
-
- let logm =
-   Config.create "svc" "localhost"
-   |> Config.target (Targets.Console.create Targets.Console.empty "my console target")
-   |> Config.processing pipeLine
-   |> Config.build
-   |> run
-
- let lg = logm.getLogger (PointName.parse "give.some.example.here")
-
- Message.event Info "MPMCQueue"
- |> Message.tag "queue"
- |> Message.tag "high-performance"
- |> Message.tag "lock-free"
- |> Message.tag "multiple-consumers"
- |> lg.logSimple
-
- ;;
I 2018-01-26T09:51:06.0523375+00:00: https://github.com/alexandrnikitin/MPMCQueue.NET [give.some.example.here]
  others:
    _logary.host => "localhost"
    _logary.service => "svc"
    _logary.tags => ["high-performance", "lock-free", "multiple-consumers", "queue"]