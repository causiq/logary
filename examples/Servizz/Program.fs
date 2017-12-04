module Servizz.Program

open Hopac
open Logary
open Logary.Targets
open Logary.Configuration
open Logary.Adapters.Facade
open System
open System.Threading
open Logary.EventsProcessing

let logger = Logging.getLoggerByName "Servizz.Program"

type Speed = Fast | Slow | OtherSpeed of speed:int
type LoginLogoutEvent =
  | UserLoggedIn of user : string * roles : string list
  | UserLoggedOut of user : string * speedAtTimeOfLeaving : Speed

type CartItem = TicTacs | GlamourMagazine | Other of name:string * price: Decimal
type CartEvent =
  | UserStartedCheckout of user : string * totalValue : decimal * items : CartItem list

[<EntryPoint>]
let main argv =
  use mre = new ManualResetEventSlim(false)
  use sub = Console.CancelKeyPress.Subscribe (fun _ -> mre.Set())
  let processing = 
    Events.stream
    |> Events.subscribers [
       Events.events
       |> Events.sink ["console";]
    ]
    |> Events.toProcessing

  let logary =
    Config.create "Servizz.Program" "localhost"
    |> Config.targets [ LiterateConsole.create LiterateConsole.empty "console" ]
    |> Config.processing processing
    |> Config.build
    |> run
    |> Registry.toLogManager

  // Initialise Libryy so it logs to Logary (proper)
  LogaryFacadeAdapter.initialise<Libryy.Logging.Logger> logary

  // if you need a Logger instance:
  let logger = logary.getLogger (PointName [| "Libryy" |])
  let librryLogger = LoggerAdapter.createGeneric logger
  
  Message.gaugeWithUnit "Test" (99./88.) Units.Seconds 
  |> logger.logSimple

  let someEventWithBinaryData = Message.templateEvent<string, byte[]> (Debug, "Some binary ({Disposition}) is {Binary}")
  someEventWithBinaryData "too big" (System.Text.Encoding.ASCII.GetBytes("hey haf, too many bytes, have to truncate!")) |> logger.logSimple
  someEventWithBinaryData "small" (System.Text.Encoding.ASCII.GetBytes("hey haf!")) |> logger.logSimple

  let userSystemEvent = Message.templateEvent<LoginLogoutEvent>(Info, "Special event {@Event}")
  userSystemEvent (UserLoggedIn ("adam", [ "admin" ])) |> logger.logSimple
  userSystemEvent (UserLoggedIn ("haf", [ "authenticatedUser"; "powerUser" ])) |> logger.logSimple
  userSystemEvent (UserLoggedIn ("mavnn", [ "general" ])) |> logger.logSimple
  userSystemEvent (UserLoggedOut ("adam", Fast)) |> logger.logSimple
  userSystemEvent (UserLoggedOut ("haf", Slow)) |> logger.logSimple
  userSystemEvent (UserLoggedOut ("mavnn", (OtherSpeed 10))) |> logger.logSimple

  let cartEvent = Message.templateEvent<CartEvent>(Info, "Cart event {@Event}")
  cartEvent (UserStartedCheckout("adam", 123.45M, [ TicTacs; Other("Book", 99.99M) ])) |> logger.logSimple
  cartEvent (UserStartedCheckout("haf", 999.99M, [ GlamourMagazine; Other("Haircut", 99.99M) ])) |> logger.logSimple

  let someCrazyTableStuff = Message.templateEvent<int, int, int, int>(Info, "Data: {c1,5} {c2,5} {c3,5} {c4,5}")
  someCrazyTableStuff 1 2 3 4 |> logger.logSimple
  someCrazyTableStuff 5 6 7 8 |> logger.logSimple
  someCrazyTableStuff 9 10 11 12 |> logger.logSimple
  someCrazyTableStuff 13 14 15 16 |> logger.logSimple

  let gotLibryyResultEvent = Message.templateEvent<int> (Debug, "Got {LibryyResult} from Libryy")
  let logLibryyResult result = gotLibryyResultEvent result |> logger.logSimple

  let workResult = Libryy.Core.work librryLogger
  logLibryyResult workResult

  let simpleWorkExnResult = Libryy.Core.generateAndLogExn librryLogger
  logLibryyResult simpleWorkExnResult

  let staticWorkResult = Libryy.Core.staticWork() |> Async.RunSynchronously
  logLibryyResult staticWorkResult

  mre.Wait()
  0
