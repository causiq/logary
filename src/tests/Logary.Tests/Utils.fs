module Logary.Tests.Utils

open System
open System.Globalization
open System.Threading
open System.IO
open Hopac
open Hopac.Infixes
open NodaTime
open Logary
open Logary.Internals
open Logary.Message
open Logary.Targets
open Logary.Configuration

let buildLogManager () = job {
  let svc = "svc"
  let host = "localhost"
  let (out, error) = (new StringWriter (),new StringWriter ())
  let twconf = TextWriter.TextWriterConf.create (out, error)
  let tname = "4test"
  let twTargetConf = TextWriter.create twconf "4test"
  // let iloggerConf = ILogger.Targets [ twTargetConf ]
  let processing =
    Events.stream
    |> Events.subscribers [
      Events.events |> Events.sink [tname]
    ]
    |> Events.toProcessing


  let! registry =
    Config.create svc host
    // |> Config.ilogger iloggerConf
    // |> Config.ilogger (ILogger.Console Verbose)
    |> Config.target twTargetConf
    |> Config.processing processing
    // |> Config.disableGlobals
    |> Config.build
  let logm = Registry.toLogManager registry
  return (registry, logm, out, error)
}