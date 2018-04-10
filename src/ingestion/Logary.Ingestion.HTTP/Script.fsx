#r "../../../packages/Suave/lib/net40/Suave.dll"
open System.IO
open Suave
open Suave.Http
open Suave.RequestErrors
open Suave.Filters
open Suave.Operators

#I "../../../packages/FParsec/lib/net40-client"
//#r "../../../packages/FParsec/lib/net40-client/FParsecCS.dll"
#r "../../../packages/FParsec/lib/net40-client/FParsec.dll"
#r "../../../packages/NodaTime/lib/net45/NodaTime.dll"

#I "bin/Debug/net461/"
#r "Logary.dll"
#r "Hopac.Core.dll"
#r "Hopac.dll"
open Logary
open Logary.EventProcessing
open Logary.Configuration
open Logary.Targets
open Hopac
#load "HTTP.fs"
open Logary.Ingestion

let root = Path.GetFullPath (Path.Combine (__SOURCE_DIRECTORY__, "app"))

let logary =
  Config.create "Logary.ConsoleApp" "localhost"
  |> Config.targets [
      LiterateConsole.create LiterateConsole.empty "console"
      Console.create Console.empty "fatal"
    ]
  |> Config.ilogger (ILogger.Console Info)
  |> Config.middleware Middleware.dnsHost
  |> Config.processing (Events.events |> Events.sink ["console"])
  |> Config.build
  |> run

let httpConf =
  { HTTP.defaultConfig with ilogger = logary.runtimeInfo.logger }

let logger = logary.getLogger (PointName [| "JS" |])

let next =
  Codecs.Codec.toJsonStringError Codecs.Codec.json
  >> Result.map logger.logSimple
  >> Job.result

startWebServer defaultConfig (
  choose [
    path "/"
      >=> Files.browseFile root "index.html"
      >=> Writers.setMimeType "text/html; charset=utf-8"

    Files.browse root

    HTTP.api httpConf next

    NOT_FOUND "Couldn't find the file you were looking for"
  ])

(* Sample input
{
  "logger": {
    "name": "logary-firefox30",
    "version": "0.5.7",
    "url": "https://github.com/logary/logary-js"
  },
  "errors": [
    {
      "type": "Error",
      "message": "Error: hello from logary-js",
      "backtrace": [
        {
          "function": "",
          "file": "http://localhost:8083/bundle.js",
          "line": 1,
          "column": 267
        },
        {
          "function": "",
          "file": "http://localhost:8083/bundle.js",
          "line": 1,
          "column": 234
        },
        {
          "function": "t",
          "file": "http://localhost:8083/bundle.js",
          "line": 1,
          "column": 102
        },
        {
          "function": "",
          "file": "http://localhost:8083/bundle.js",
          "line": 1,
          "column": 194
        },
        {
          "function": "",
          "file": "http://localhost:8083/bundle.js",
          "line": 1,
          "column": 1
        }
      ]
    }
  ],
  "context": {
    "language": "JavaScript",
    "userAgent": "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.10; rv:38.0) Gecko/20100101 Firefox/38.0",
    "url": "http://localhost:8083/"
  },
  "data": {},
  "session": {}
}
*)
