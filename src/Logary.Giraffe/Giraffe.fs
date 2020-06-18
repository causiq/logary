namespace Logary.Giraffe

open System
open System.Collections.Generic
open System.IO
open System.Linq
open System.Threading.Tasks
open Microsoft.AspNetCore
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.Configuration
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.Logging
open Hopac

type WebServer private () =
  static member startWebServer(?configureLogary: _, ?configureWebServer, ?args) =
    let args = defaultArg args [||]
    Alt.fromUnitTask <| fun ct ->
    Host.CreateDefaultBuilder(args)
        .ConfigureWebHostDefaults(fun webBuilder ->
          webBuilder.UseStartup<Startup>()
          |> ignore)
        .Build()
        .StartAsync(ct)
