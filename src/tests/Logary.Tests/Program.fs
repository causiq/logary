module Program

open System.Globalization
open System.Threading
open Fuchu
open System

[<EntryPoint>]
let main args =
  let enUS = CultureInfo "en-US"
  Thread.CurrentThread.CurrentCulture   <- enUS
  Thread.CurrentThread.CurrentUICulture <- enUS
  defaultMainThisAssembly args
