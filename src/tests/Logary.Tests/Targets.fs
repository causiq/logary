module Logary.Tests.Targets

open System
open Logary
open Logary.Target

let env k =
  match Environment.GetEnvironmentVariable k with
  | null ->
    Choice2Of2 (sprintf "Key '%s' not found in environment" k)
  | v ->
    Choice1Of2 v

let basicTests (conf : TargetConf) =
  ()

let integrationTests (conf : TargetConf) =
  ()