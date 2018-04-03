module Logary.Targets.Mixpanel.Tests.Program

open System
open System.Net.Mail
open Expecto
open Expecto.Flip
open NodaTime
open Hopac
open Hopac.Infixes
open Logary
open Logary.Internals
open Logary.Targets.Mixpanel

let env k =
  match Environment.GetEnvironmentVariable k with
  | null -> failwithf "couldn't load key %s" k
  | v -> v

let start () =
  // You'll have to set the environment var MIXPANEL_TOKEN to run this;
  // but besides that, this is the only configuration you need:
  MixpanelConf.create(env "MIXPANEL_TOKEN")

// TODO: Influx/Elmah-like basic tests

[<EntryPoint>]
let main argv =
  Tests.runTestsInAssembly defaultConfig argv