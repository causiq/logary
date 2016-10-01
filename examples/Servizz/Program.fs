module Servizz.Program

open Hopac
open Logary
open Logary.Targets
open Logary.Configuration
open Logary.Adapters.Facade
open System
open System.Threading

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

  use logary =
    withLogaryManager "Servizz.Program" (
      withTargets [ LiterateConsole.create LiterateConsole.empty "console" ]
      >> withRules [ Rule.createForTarget "console" ])
    |> run

  // Initialise Libryy so it logs to Logary (proper)
  LogaryFacadeAdapter.initialise<Libryy.Logging.Logger> logary

  // if you need a Logger instance:
  let logger = logary.getLogger (PointName [| "Libryy" |])
  let librryLogger = LoggerAdapter.createGeneric logger
  
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

  let gotLibryyResultEvent = Message.templateEvent<int> (Debug, "Got {LibryyResult} from Libryy")
  let logLibryyResult result = gotLibryyResultEvent result |> logger.logSimple

  let workResult = Libryy.Core.work librryLogger
  logLibryyResult workResult

  let simpleWorkExnResult = Libryy.Core.generateAndLogExn librryLogger
  logLibryyResult simpleWorkExnResult

  let staticWorkResult = Libryy.Core.staticWork()
  logLibryyResult staticWorkResult

  mre.Wait()
  0
