namespace Logary.Giraffe

open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.Hosting
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
