#r "../../../packages/Suave/lib/net40/Suave.dll"
open Suave.Web
open Suave.Http
open Suave.RequestErrors
open Suave.Filters

#I "../../../packages/FParsec/lib/net40-client"
//#r "../../../packages/FParsec/lib/net40-client/FParsecCS.dll"
#r "../../../packages/FParsec/lib/net40-client/FParsec.dll"
#r "../../../packages/Aether/lib/net40/Aether.dll"
#r "../../../packages/Chiron/lib/net45/Chiron.dll"
#r "../../../packages/NodaTime/lib/net35-Client/NodaTime.dll"

#I "bin/Debug"
#r "bin/Debug/Logary.dll"
open Logary
open Logary.Configuration
open Logary.Targets
#load "SuaveReporter.fs"
open Logary.Services

open System.IO

let root = Path.GetFullPath "./app"

let logary =
  withLogary' "Logary.Services.SuaveReporter" (
    withTargets [
      Console.create Console.empty "console"
    ] >>
    withRules [
      Rule.createForTarget "console"
    ]
  )

startWebServer defaultConfig (
  choose [
    path "/" >>= Files.browseFile root "index.html"
    Files.browse root
    SuaveReporter.api (logary.GetLogger "Logary.Services.SuaveReporter") None
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