module Program

open System.Globalization
open System.Threading
open Fuchu

[<EntryPoint>]
let main args =
  let enUS = CultureInfo "en-US"
  Thread.CurrentThread.CurrentCulture   <- enUS
  Thread.CurrentThread.CurrentUICulture <- enUS
  defaultMainThisAssembly args
  //Logary.Tests.Registry.registry |> Tests.run