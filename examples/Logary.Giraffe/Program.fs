module Logary.Giraffe.App

open System
open System.IO
open System.Net.Http
open Logary
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
// Models
// ---------------------------------

type Message =
    {
        Text: string
    }

// ---------------------------------
// Views
// ---------------------------------

module Views =
    open GiraffeViewEngine

    let layout (content: XmlNode list) =
        html [] [
            head [] [
                title [] [ encodedText "Logary.Giraffe" ]
                link [ _rel "stylesheet"
                       _type "text/css"
                       _href "/main.css" ]
            ]
            body [] content
        ]

    let partial() =
        h1 [] [ encodedText "Logary.Giraffe" ]

    let index (model: Message) =
        [
            partial()
            p [] [ encodedText model.Text ]
        ] |> layout

// ---------------------------------
// Web app
// ---------------------------------

let indexHandler (name: string) =
  let greetings = sprintf "Hello %s, from Logary!" name
  let model = { Text = greetings }
  let view = Views.index model
  htmlView view

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
      let span = ctx.getOrCreateSpanLogger "traceStateHandler"
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

let configureApp logger (app: IApplicationBuilder) =
    let env = app.ApplicationServices.GetService<IWebHostEnvironment>()
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

let configureLogging (builder: ILoggingBuilder) =
    builder.AddConsole()
      |> ignore

[<EntryPoint>]
let main _ =
    let logary =
      Config.create "Logary.Giraffe" "laptop"
      |> Config.target (Console.create Console.empty "console")
      |> Config.ilogger (ILogger.Console Debug)
      |> Config.build
      |> Hopac.Hopac.run

    let logger = logary.getLogger "Logary.Giraffe"
    let contentRoot = Directory.GetCurrentDirectory()
    let webRoot = Path.Combine(contentRoot, "public")
    WebHostBuilder()
        .UseKestrel()
        .UseLogary(logary)
        .UseContentRoot(contentRoot)
        .UseWebRoot(webRoot)
        .Configure(Action<IApplicationBuilder>(configureApp logger))
        .ConfigureServices(configureServices)
        .ConfigureLogging(configureLogging)
        .Build()
        .Run()
    0
