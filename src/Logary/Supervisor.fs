module Logary.Supervisor

open System
open Hopac
open NodaTime
open Logary
open Logary.Internals
open Logary.Utils

type internal SupervisorState =
  Map<string, uint32>

type SupervisorMessage =
  /// Register the given job to be supervised. The job will be automatically
  /// restarted if it crashes. A given job name can only be registered once.
  | Register of NamedJob<unit>
  /// Removes a job from the supervision list. Used by the supervisor loop to
  /// signal that the job has finished without error.
  | Unregister of name:string
  /// Used by a supervisor job to report crashes.
  /// The supervisor sets the IVar to indicate if the job should be restarted.
  | JobErrored of name:string * exn:exn option * shouldRestart:IVar<bool>

type Options =
  { logger       : Logger
    maxRestarts  : uint32
    restartDelay : Duration }

  static member create(logger: Logger, ?maxRestarts: uint32, ?restartDelay : Duration) =
    { logger       = logger
      maxRestarts  = defaultArg maxRestarts 10u
      restartDelay = defaultArg restartDelay (Duration.FromMilliseconds 500L) }

type Instance =
  { options : Options
    ch      : Ch<SupervisorMessage> }

let private log =
  let supervisorName = PointName [| "Logary"; "Supervisor" |]
  fun instance ->
    Message.setName supervisorName
    >> Logger.log instance.options.logger

/// Register the given job to be supervised. The job will be automatically
/// restarted if it crashes. A job can only be registered once.
let register instance namedJob =
  Ch.give instance.ch (Register namedJob) :> Job<_>

/// Supervision loop for a single job.
let rec private supervise instance (nj : NamedJob<_>) = job {
  // Block until the job finishes or crashes.
  let! result = Job.catch nj.job
  match result with
  | Choice1Of2 _ ->
    // If the job is finished without an exception (presumably shut down),
    // stop supervising.
    do! Message.eventInfof "Job '%s' has quit and will be unregistered." nj.name
        |> log instance
    do! Ch.give instance.ch (Unregister nj.name)

  | Choice2Of2 ex ->
    // Ask the supervisor if the job should be restarted
    let! shouldRestart = IVar ()
    // TODO: do we ever NOT feed JobErrored an exception?
    do! JobErrored (nj.name, Some ex, shouldRestart) |> Ch.give instance.ch
    let! restart = IVar.read shouldRestart

    if restart then
      do! timeOut (instance.options.restartDelay.ToTimeSpan())
      return! supervise instance nj
    else
      do! Unregister nj.name |> Ch.give instance.ch
}

/// Main supervisor loop.
let private loop (instance : Instance) =
  let log = log instance

  let rec f (jobs: SupervisorState) = job {
    let! msg = Ch.take instance.ch
    match msg with
    | Register nj ->
      // A job can only be registered once.
      if jobs |> Map.containsKey nj.name then
        failwithf "Job \"%s\" has already been registered." nj.name

      do! Message.eventInfof "Now supervising '%s'." nj.name |> log
      do! Job.start (supervise instance nj)
      return! f (Map.add nj.name 0u jobs)

    | Unregister name ->
      if Map.containsKey name jobs then
        return! f (Map.remove name jobs)
      else
        return! f jobs

    | JobErrored (name, exn, shouldRestart) ->
      let restarted = Map.find name jobs

      // Attach an exception if we have one
      let addExn =
        match exn with
        | Some ex -> Message.addExn ex
        | None    -> id
      let log = addExn >> log

      match restarted, instance.options.maxRestarts with
      | r, mr when r > mr ->
        do! Message.eventErrorf "Job \"%s\" has crashed too many times and won't be restarted." name |> log
        do! IVar.fill shouldRestart false
        return! f jobs

      | _ ->
        do! Message.eventErrorf "Job \"%s\" has crashed and will be restarted." name |> log
        do! IVar.fill shouldRestart true
        return! f (Map.add name (restarted + 1u) jobs)
  }

  f Map.empty

let create options =
  { options = options
    ch      = Ch () }

let start instance =
  server (loop instance)
