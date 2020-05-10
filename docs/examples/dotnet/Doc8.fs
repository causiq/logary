let someRuleOnTarget =
  Rule.empty
  |> Rule.setMinLevel LogLevel.Error // this target will only get message about error level (inclusive)
  |> Rule.setPath (System.Text.RegularExpressions.Regex("a.b.c.*")) // only accept message name under a.b.cxxxxx
  |> Rule.setAcceptIf (fun msg -> msg |> Message.hasTag "emergency")

let tconf =
  Targets.LiterateConsole.create Targets.LiterateConsole.empty "nice console"
  |> TargetConf.addRule someRuleOnTarget
let logm =
  Config.create "svc" "localhost"
  |> Config.target tconf
  |> Config.loggerMinLevel "a.b.*" LogLevel.Fatal  // logger under a.bxxxx path only process Fatal message
  |> Config.loggerMinLevel "a.b.c.*" LogLevel.Info // logger under a.b.cxxxx path can process message above Info
  |> Config.build
  |> run

let abc = logm.getLogger (PointName.parse "a.b.cxxx")
let ab = logm.getLogger (PointName.parse "a.bxxx")

abc.verbose (Message.eventX "abc.Info" >> fun msg -> printfn "invoke %s" msg.value; msg) // no invoke
abc.error (Message.eventX "abc.Error" >> fun msg -> printfn "invoke %s" msg.value; msg) // invoke, but will not go to target
abc.error (Message.eventX "abc.Error with emergency tag" >> (fun msg -> printfn "invoke%s" msg.value; msg) >> Message.tag "emergency") // hurray

ab.error (Message.eventX "ab.Error" >> (fun msg -> printfn "invoke %s" msg.value; msg) >> Message.tag "emergency") // no invoke
ab.fatal (Message.eventX "ab.Fatal" >> (fun msg -> printfn "invoke %s" msg.value; msg) >> Message.tag "emergency") // hurray

> abc.verbose (Message.eventX "abc.Info" >> fun msg -> printfn "invoke %s" msg.value; msg) // no invoke
- ;;
val it : unit = ()

> abc.error (Message.eventX "abc.Error" >> fun msg -> printfn "invoke %s" msg.value; msg) // invoke, but will not go to target
- ;;
invoke abc.Error
val it : unit = ()

> abc.error (Message.eventX "abc.Error with emergency tag" >> (fun msg -> printfn "invoke%s" msg.value; msg) >> Message.tag "emergency") // hurray
- ;;
invokeabc.Error with emergency tag
val it : unit = ()

> [19:06:33 ERR] abc.Error with emergency tag <a.b.cxxx>
  others:
    _logary.host => "localhost"
    _logary.service => "svc"
    _logary.tags => ["emergency"]

> ab.error (Message.eventX "ab.Error" >> (fun msg -> printfn "invoke %s" msg.value; msg) >> Message.tag "emergency") // no invoke
- ;;
val it : unit = ()

> ab.fatal (Message.eventX "ab.Fatal" >> (fun msg -> printfn "invoke %s" msg.value; msg) >> Message.tag "emergency") // hurray
- ;;
invoke ab.Fatal
[val19:07:45  FTLit]  ab.Fatal:  unit<a.bxxx>
  others:
    _logary.host => "localhost"
    =_logary.service => "svc"
    _logary.tags  => ["emergency"]