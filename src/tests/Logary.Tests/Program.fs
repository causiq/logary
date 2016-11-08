module Logary.Tests.Program

open System.Globalization
open System.Threading
open Expecto
open Hopac
open Hopac.Infixes
open Logary
open Logary.Adapters.Facade

[<EntryPoint>]
let main args =
  let enUS = CultureInfo "en-US"
  let adapter = LoggerAdapter.createGeneric<Expecto.Logging.Logger> Fac.literal.Value
  Expecto.Logging.Global.initialise { Expecto.Logging.Global.DefaultConfig with getLogger = fun _ -> adapter }
  Thread.CurrentThread.CurrentCulture   <- enUS
  Thread.CurrentThread.CurrentUICulture <- enUS
  Tests.runTestsInAssembly defaultConfig args