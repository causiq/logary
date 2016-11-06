module Program

open System.Globalization
open System.Threading
open Expecto
open Hopac
open Hopac.Infixes
open Logary
open Logary.Adapters.Facade
open Logary.Internals
open Logary.Tests.Fac
open Logary.Targets

[<EntryPoint>]
let main args =
  let enUS = CultureInfo "en-US"
  let literal =
    LiterateConsole.create LiterateConsole.empty "literate"
    |> Target.init { emptyRuntime with serviceName = "tests" }
    >>= (fun ti -> Job.start (ti.server (fun _ -> Job.result ()) None) >>-. ti)
    >>- (Seq.singleton >> InternalLogger.create Debug)
    |> run
  let adapter = LoggerAdapter.createGeneric<Expecto.Logging.Logger> literal
  Expecto.Logging.Global.initialise { Expecto.Logging.Global.DefaultConfig with getLogger = fun _ -> adapter }
  Thread.CurrentThread.CurrentCulture   <- enUS
  Thread.CurrentThread.CurrentUICulture <- enUS
  Tests.runTestsInAssembly defaultConfig args
