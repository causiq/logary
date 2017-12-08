/// The `Config` module handles interaction with the Registry by building the
/// required configuration for Logary.
namespace Logary.Configuration

open Hopac
open Hopac.Infixes
open Hopac.Extensions
open Logary
open Logary.Internals
open Logary.Targets
open Logary.EventsProcessing


/// Specifies the internal logging level for Logary.
[<RequireQualifiedAccess>]
type ILogger =
  | Console of minLevel:LogLevel
  | LiterateConsole of minLevel:LogLevel
  | Targets of config:TargetConf list

module Config =
  type T =
    private {
      targets      : HashMap<string, TargetConf>
      host         : string
      service      : string
      getTimestamp : unit -> EpochNanoSeconds
      getSem       : unit -> obj
      ilogger      : ILogger
      middleware   : Middleware list
      processing   : Events.Processing
      setGlobals   : bool
    }

  let create service host =
    { targets      = HashMap.empty
      host         = host
      service      = service
      getTimestamp = Global.getTimestamp
      getSem       = Global.getConsoleSemaphore
      middleware   = List.empty
      ilogger      = ILogger.Console Warn
      setGlobals   = true
      processing   = Events.events
    }

  let target tconf lconf =
    { lconf with targets = lconf.targets |> HashMap.add tconf.name tconf }

  let targets tconfs lconf =
    tconfs |> Seq.fold (fun lconf tconf -> lconf |> target tconf) lconf

  let host host lconf =
    { lconf with host = host }

  let service name lconf =
    { lconf with service = name }

  let timestamp getTimestamp lconf =
    { lconf with getTimestamp = getTimestamp }

  let consoleSemaphore getConsoleSemaphore lconf =
    { lconf with getSem = getConsoleSemaphore }

  let middleware mid (lconf : T) =
    { lconf with middleware = mid :: lconf.middleware }

  let ilogger ilogger lconf =
    { lconf with ilogger = ilogger }

  let processing processor lconf =
    { lconf with processing = processor }

  let disableGlobals lconf =
    { lconf with setGlobals = false }

  let private initFormatting () =
    Global.Destructure.addDestructure<Gauge> <| fun req ->
      let gauge = req.Value
      let (Gauge (value, units)) = gauge
      let (scaledValue, unitsFormat) = Units.scale units value
      if String.IsNullOrEmpty unitsFormat then ScalarValue scaledValue
      else ScalarValue (sprintf "%s %s" (string scaledValue) unitsFormat)

  let build (lconf : T) : Job<Registry.T> =
    initFormatting ()

    let ri : RuntimeInfo.T =
      { service = lconf.service
        host = lconf.host
        getTimestamp = lconf.getTimestamp
        getConsoleSemaphore = lconf.getSem
        logger = NullLogger.instance }

    let itargets =
      match lconf.ilogger with
      | ILogger.Console minLevel ->
        let target = Console.create Console.empty "internal"
        let rule = Rule.empty |> Rule.setMinLevel minLevel
        [TargetConf.setRule rule target]

      | ILogger.LiterateConsole minLevel ->
        let target = LiterateConsole.create LiterateConsole.empty "internal"
        let rule = Rule.empty |> Rule.setMinLevel minLevel
        [TargetConf.setRule rule target]

      | ILogger.Targets conf ->
        conf

    InternalLogger.create ri >>= fun ilogger ->
    itargets
    |> Seq.Con.iterJob (fun itarget -> InternalLogger.add itarget ilogger)
    >>= fun () ->
    let mids =
      [ Middleware.host lconf.host
        Middleware.service lconf.service ]
    let middleware = Array.ofList (lconf.middleware @ mids)
    let ri = { ri with logger = ilogger }

    let conf =
      { new LogaryConf with
          member x.targets = lconf.targets
          member x.runtimeInfo = upcast ri
          member x.middleware = middleware
          member x.processing = lconf.processing
      }
    Registry.create conf

  