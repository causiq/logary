module Logary.Giraffe.App

open System
open System.IO
open System.Net.Http
open Logary
open Logary.Model
open Logary.Targets
open Logary.Configuration
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Cors.Infrastructure
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.Logging
open Microsoft.Extensions.DependencyInjection
open Giraffe
open FSharp.Control.Tasks.V2

// ---------------------------------
// Web app
// ---------------------------------

let indexHandler (name: string) =
  sprintf "Hello %s, from Logary!" name
    |> Successful.OK

type InputItem =
  { arguments: string[]; url: string }


open Logary.Trace
open Logary.Trace.Propagation
open Logary.Internals.Chiron

type J = Logary.Internals.Chiron.Json

let tracestateHandler (inputs: InputItem[]): HttpHandler =
  // CancellationToken docs only available for Controllers !??!?
  let c = new HttpClient()
  fun next ctx ->
    task {
      let span = ctx.getCreateLogger "traceStateHandler"
      for input in inputs do
        let body = input.arguments |> Seq.map J.String |> Seq.toList |> J.Array |> Json.format
        let message = new HttpRequestMessage(HttpMethod.Post, input.url)
        message.Content <- new StringContent(body)

        use span = span.startChild("Call TestSuite")

        use! res =
          message
            |> span.injectWith (W3C.propagator, Inject.httpRequestMessage)
            |> c.SendAsync

        ctx.logger.info("Received response code={code} from test suite", fun m ->
          m.setField("code", int res.StatusCode))

      return! text "All sent!" next ctx
    }

let webApp =
  choose [
    GET >=> choose [
      route "/" >=> indexHandler "Logary"
      routef "/hello/%s" indexHandler
      route "/error" >=> (fun _ _ -> failwith "Something went wrong!")
    ]
    POST >=> route "/w3c/tracestate" >=> bindModel<InputItem[]> None tracestateHandler
    setStatusCode 404 >=> text "Not Found"
  ]

// ---------------------------------
// Error handler
// ---------------------------------

let errorHandler (logger: Logger) (ex: Exception) _ =
  logger.error("An unhandled exception has occurred while executing the request.", ex)
  clearResponse >=> setStatusCode 500 >=> text ex.Message

// ---------------------------------
// Config and Main
// ---------------------------------

let configureCors (builder: CorsPolicyBuilder) =
    builder.WithOrigins("http://localhost:5000,https://localhost:5001")
           .AllowAnyMethod()
           .AllowAnyHeader()
           |> ignore

let configureApp (logary: LogManager) (app: IApplicationBuilder) =
    let env = app.ApplicationServices.GetService<IWebHostEnvironment>()
    let logger = logary.getLogger "Logary.Examples.Giraffe"

    (if env.EnvironmentName = "Development"
     then
      app.UseDeveloperExceptionPage()
     else
      app.UseGiraffeErrorHandler(errorHandler logger))
        .UseCors(configureCors)
        .UseStaticFiles()
        .UseGiraffe(webApp)

let configureServices (services: IServiceCollection) =
    services.AddCors().AddGiraffe()
      |> ignore

[<EntryPoint>]
let main _ =
  let logary =
    Resource.create("Logary.Giraffe", "laptop")
    |> Config.create
    |> Config.target (Console.create Console.empty "console")
    |> Config.ilogger (ILogger.Console Debug)
    |> Config.build
    |> Hopac.Hopac.run

  let contentRoot = Directory.GetCurrentDirectory()
  let webRoot = Path.Combine(contentRoot, "public")
  WebHostBuilder()
    .UseKestrel()
    .UseLogary(logary)
    .UseContentRoot(contentRoot)
    .UseWebRoot(webRoot)
    .Configure(Action<IApplicationBuilder>(configureApp logary))
    .ConfigureServices(configureServices)
    .Build()
    .Run()
  0
