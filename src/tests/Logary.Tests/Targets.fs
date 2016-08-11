module Logary.Tests.Targets

open System
open Fuchu
open Hopac
open Logary
open ExpectoPatronum

let env k (parser : string -> 't) : Choice<'t, string> =
  match Environment.GetEnvironmentVariable k with
  | null ->
    Choice2Of2 (sprintf "Key '%s' not found in environment" k)
  | v ->
    Choice1Of2 (parser v)

let envForce k (parser : string -> 't) : 't =
  match env k parser with
  | Choice1Of2 value ->
    value
  | Choice2Of2 error ->
    failwith error

module Target =

  /// Send the message to the target and wait for it to ack it.
  let logAndWait (target : Target.TargetInstance) message =
    Target.log target message
    |> run 
    |> run

  /// Finalise the target and assert it was finalised within 1000 milliseconds
  let finalise (target : Target.TargetInstance) =
    Alt.choose [
      Target.shutdown target
        |> Alt.afterFun Internals.Success

      timeOutMillis 1000
        |> Alt.afterFun (fun _ -> Internals.TimedOut)
    ]
    |> run
    |> function
    | Internals.TimedOut ->
      Tests.failtestf "finalising target timed out: %A" target

    | Internals.Success _ ->
      ()

/// Run basic tests against the target;
///
/// - can create instance thereof
/// - can start and stop
/// - can receive a few different sorts of messages
let basicTests targetName (configFactory : string -> Target.TargetConf) =
  testList (sprintf "basic tests for %s" targetName) [
    testCase "creating instance" <| fun _ ->
      let target =
        Target.confTarget "basic1" configFactory

      let instance =
        target.initer Fac.emptyRuntime |> run

      Expect.equal instance.name
                   (PointName.ofSingle "basic1")
                   "Should be named 'basic1'"

      Expect.isNotNull instance.server "Should have Job<unit> as #server"

    testCase "start and stop" <| fun _ ->
      let target =
        Target.confTarget "basic2" configFactory

      let instance =
        target |> Target.init Fac.emptyRuntime |> run

      start instance.server

      try
        Message.eventInfo "Hello World!"
        |> Target.logAndWait instance

      finally
        Target.finalise instance
  ]

let integrationTests targetName (configFactory : string -> Target.TargetConf) =
  testList (sprintf "integration tests for %s" targetName) []