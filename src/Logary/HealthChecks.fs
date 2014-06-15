namespace Logary

/// A module that makes it smooth to interact with running/starting/configuration of
/// health checks.
module HealthChecks =
  open FSharp.Actor

  // success is next to type HealthCheck (Healthy)
  type Failure =
    { message : string
      ``exception`` : exn option }
    interface UnhealthyResult with
      member x.Message = x.message
      member x.Exception = x.``exception``
    static member Create(msg, ?e) =
      let e = defaultArg e None
      { message = msg; ``exception`` = e } :> UnhealthyResult

  type FnCheckerMessage =
    | RunCheck

  type HealthCheckInstance =
    { actor : IActor<FnCheckerMessage> }

  type private FnCheckerState =
    { last : HealthCheckResult }

  let private createFnChecker fn slot =
    (fun (inbox : IActor<_>) ->
      let rec running state =
        async {
          let! RunCheck, mopts = inbox.Receive()
          let! last' = fn ()
          slot := last'
          return! running { state with last = last' } }
      running { last = Healthy })

  let fromFn name f =
    let slot = ref Healthy
    let a = Actor.spawn (Actor.Options.Create(sprintf "logaryRoot/healthCheck/%s" name)) (createFnChecker f slot)
    a.Post(RunCheck, None)
    { new HealthCheck with
      member x.Name = name
      member x.Check() = !slot }
