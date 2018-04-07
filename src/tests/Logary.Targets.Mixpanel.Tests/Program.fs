module Logary.Targets.Mixpanel.Tests.Program

open System
open System.Net.Mail
open Expecto
open Expecto.Flip
open NodaTime
open Hopac
open Hopac.Infixes
open Logary
open Logary.Tests
open Logary.Internals
open Logary.Targets.Mixpanel

let conf =
  // You'll have to set the environment var MIXPANEL_TOKEN to run this;
  // but besides that, this is the only configuration you need:
  MixpanelConf.create "ec080ace2f4d2aca519dc70b5e1cd4ea"

[<Tests>]
let tests =
  TargetBaseline.basicTests "Mixpanel" (create conf) false

  // TODO: handle 503s from Mixpanel


[<EntryPoint>]
let main argv =
  Tests.runTestsInAssembly defaultConfig argv