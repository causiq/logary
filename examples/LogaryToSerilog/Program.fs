
open System
open System.Threading
open Hopac
open Logary
open Logary.Configuration
open Logary.Targets
open Serilog

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
  use serilogLogger = Serilog.LoggerConfiguration()
                        .MinimumLevel.Verbose()
                        .WriteTo.Seq("http://localhost:5341")
                        .CreateLogger()

  let serilogConfig =
    { SerilogTarget.empty with logger = serilogLogger }

  use logary =
    withLogaryManager "Servizz.Program" (
      withTargets [
        LiterateConsole.create LiterateConsole.empty "console"
        SerilogTarget.create serilogConfig "serilog"
      ]
      >> withRules [
        Rule.createForTarget "console"
        Rule.createForTarget "serilog"
      ])
    |> run

  let logger = Logging.getLoggerByName "LogaryToSerilog"
  
  Message.gaugeWithUnit (PointName.ofSingle "Test") Units.Seconds (Value.Fraction (99L, 88L))
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

  mre.Wait()
  0 // return an integer exit code
