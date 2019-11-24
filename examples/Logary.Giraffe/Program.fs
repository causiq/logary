module Logary.Giraffe.App

open System
open System.IO
open Logary
open Logary.Message
open Logary.Configuration
open Logary.Targets
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Cors.Infrastructure
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.Logging
open Microsoft.Extensions.DependencyInjection
open Giraffe

// ---------------------------------
// Models
// ---------------------------------

type Message =
    {
        Text : string
    }

// ---------------------------------
// Views
// ---------------------------------

module Views =
    open GiraffeViewEngine

    let layout (content: XmlNode list) =
        html [] [
            head [] [
                title []  [ encodedText "Logary.Giraffe" ]
                link [ _rel  "stylesheet"
                       _type "text/css"
                       _href "/main.css" ]
            ]
            body [] content
        ]

    let partial () =
        h1 [] [ encodedText "Logary.Giraffe" ]

    let index (model : Message) =
        [
            partial()
            p [] [ encodedText model.Text ]
        ] |> layout

// ---------------------------------
// Web app
// ---------------------------------

let indexHandler (name: string) =
    let greetings = sprintf "Hello %s, from Giraffe!" name
    let model     = { Text = greetings }
    let view      = Views.index model
    htmlView view

let webApp =
    choose [
        GET >=>
            choose [
                route "/" >=> indexHandler "world"
                routef "/hello/%s" indexHandler
            ]
        setStatusCode 404 >=> text "Not Found" ]

// ---------------------------------
// Error handler
// ---------------------------------

let errorHandler (logger: Logger) (ex: Exception) _ =
    logger.error (eventX "An unhandled exception has occurred while executing the request." >> addExn ex)
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
      |> Config.target (LiterateConsole.create LiterateConsole.empty "console")
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
        .Configure(Action<IApplicationBuilder> (configureApp logger))
        .ConfigureServices(configureServices)
        .ConfigureLogging(configureLogging)
        .Build()
        .Run()
    0