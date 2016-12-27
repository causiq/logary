module Logary.Supervisor

open Logary.Internals
open Hopac
open Hopac.Infixes

type Will<'a> = Will of MVar<'a option>

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Will =
  let create () : Will<'a> =
    Will (MVar None)
  let createFull initial : Will<'a> =
    Will (MVar (Some initial))
  let update (Will aM) (a:'a) : Alt<unit> =
    MVar.mutateFun (always (Some a)) aM
  let latest (Will aM) : Alt<'a option> =
    MVar.read aM
  let exchange (Will aM) (a:'a) : Alt<'a option> =
    MVar.modifyFun (fun a' -> Some a, a') aM
  let revoke (Will aM) : Alt<unit> =
    MVar.mutateFun (always None) aM

type Policy =
  | Always of FailureAction
  | DetermineWith of (exn -> FailureAction)
  | DetermineWithJob of (exn -> Job<FailureAction>)

and FailureAction =
  | Restart
  | RestartDelayed of restartDelayMs:uint32
  | Terminate
  | Escalate

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Policy =
  let restart = Always Restart
  let restartDelayed delay = Always ^ RestartDelayed delay
  let terminate = Always Terminate
  let escalate = Always Escalate

  let retry (maxRetries : uint32) =
    let cM = MVar 1u
    DetermineWithJob ^ fun _ ->
      MVar.modifyFun (fun r -> r + 1u, r) cM
      >>- fun r ->
        if r = maxRetries then
          Terminate
        else
          Restart

  let retryWithDelay (d : uint32) (maxRetries : uint32) =
    let cM = MVar 1u
    DetermineWithJob ^ fun _ ->
      MVar.modifyFun (fun r -> r + 1u, r) cM
      >>- fun r ->
        if r = maxRetries then
          Terminate
        else
          RestartDelayed d

  let exponentialBackoff (initD : uint32) (mult : uint32) (maxD : uint32) (maxRetries : uint32) =
    let doRestart =
      match initD, mult, maxD with
      | 0u, _, _
      | _, 0u, _ -> always Restart
      | _, _, _ -> fun r -> RestartDelayed (min maxD (initD * pown mult (int r)))
    let cM = MVar 1u
    DetermineWithJob ^ fun _ ->
      MVar.modifyFun (fun r -> r + 1u, r) cM
      >>- fun r ->
        if r = maxRetries then
          Terminate
        else
          doRestart r

type SupervisedJob<'a> = Job<Choice<'a,exn>>

module Job =
  let rec handleFailureWith p act (xJ : #Job<'x>) (ex : exn) : SupervisedJob<'x> =
    match act with
    | Restart ->
      supervise p xJ
    | RestartDelayed t ->
      timeOutMillis (int t) >>= fun () -> supervise p xJ
    | Terminate -> 
      Choice2Of2 ex |> Job.result
    | Escalate ->
      Job.raises ex

  and makeHandler (p : Policy) : #Job<'x> -> exn -> SupervisedJob<'x> =
    match p with
    | Always act ->
      handleFailureWith p act
    | DetermineWith e2act ->
      fun xJ ex ->
        handleFailureWith p (e2act ex) xJ ex
    | DetermineWithJob e2actJ ->
      fun xJ ex ->
        e2actJ ex >>= fun act -> handleFailureWith p act xJ ex

  and supervise (p : Policy) (xJ : #Job<'x>) : SupervisedJob<'x> =
    let handle = makeHandler p
    Job.tryIn xJ (Choice1Of2 >> Job.result) (handle xJ)

  let superviseWithWill p w2xJ =
    let wl = Will.create ()
    supervise p (w2xJ wl)