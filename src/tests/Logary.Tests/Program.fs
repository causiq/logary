module Program

open Fuchu

[<EntryPoint>]
let main args =
  System.Threading.Thread.CurrentThread.CurrentCulture <- System.Globalization.CultureInfo.GetCultureInfo("en-US")
  System.Threading.Thread.CurrentThread.CurrentUICulture<- System.Globalization.CultureInfo.GetCultureInfo("en-US")
  //defaultMainThisAssembly args
  Logary.Tests.Registry.registry
  |> Tests.run