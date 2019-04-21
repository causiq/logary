let pipeLine =
   Events.events
   |> Events.tag "queue"
   |> Pipe.map (fun msg -> {msg with value = "https://github.com/alexandrnikitin/MPMCQueue.NET"})
   |> Events.sink ["nice console"]

let logm =
  Config.create "svc" "localhost"
  |> Config.target (Targets.Console.create Targets.Console.empty "my console target")
  |> Config.target (Targets.LiterateConsole.create Targets.LiterateConsole.empty "nice console")
  |> Config.processing pipeLine
  |> Config.build
  |> run

let lg = logm.getLogger (PointName.parse "give.some.example.here")

Message.event Info "MPMCQueue"
|> Message.tag "queue"
|> Message.tag "high-performance"
|> Message.tag "lock-free"
|> Message.tag "multiple-consumers"
|> lg.logSimple