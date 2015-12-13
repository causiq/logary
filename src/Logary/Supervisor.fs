module Logary.Supervisor

open System
open Hopac
open NodaTime
open Logary
open Logary.Internals
open Logary.Utils

type internal SupervisorState =
  Map<PointName, uint32>

type SupervisorMessage =
  /// Register the given job to be supervised. The job will be automatically
  /// restarted if it crashes. A given job name can only be registered once.
  | Register of NamedJob<unit>
  /// Removes a job from the supervision list. Used by the supervisor loop to
  /// signal that the job has finished without error.
  | Unregister of name:PointName
  /// Used by a supervisor job to report crashes.
  /// The supervisor sets the IVar to indicate if the job should be restarted.
  | JobErrored of name:PointName * exn:exn option * shouldRestart:IVar<bool>

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

let private log instance =
  Message.setName (PointName.ofList ["Logary"; "Supervisor"])
  >> instance.options.logger.log

/// Register the given job to be supervised. The job will be automatically
/// restarted if it crashes. A job can only be registered once.
let register instace namedJob = job {
  do! Ch.give instace.ch (Register namedJob)
}

/// Supervision loop for a single job.
let rec private supervise instance (nj : NamedJob<_>) = job {
  // Block until the job finishes or crashes.
  let! result = Job.catch nj.job
  match result with
  | Choice1Of2 _ ->
    // If the job is finished without an exception (presumably shut down),
    // stop supervising.
    do! Ch.give instance.ch (Unregister (PointName.ofList nj.name))
    ()

  | Choice2Of2 ex ->
    // Ask the supervisor if the job should be restarted
    let! shouldRestart = IVar.create ()
    // TODO: do we ever NOT feed JobErrored an exception?
    do! JobErrored (PointName nj.name, Some ex, shouldRestart) |> Ch.give instance.ch
    let! restart = IVar.read shouldRestart

    if restart then
      do! timeOut (instance.options.restartDelay.ToTimeSpan())
      return! supervise instance nj
    else
      do! Unregister (PointName nj.name) |> Ch.give instance.ch
}

/// Main supervisor loop.
let private loop (instance : Instance) =
  let log = log instance

  let rec f (jobs: SupervisorState) = job {
    let! msg = Ch.take instance.ch
    match msg with
    | Register nj ->
      // A job can only be registered once.
      if jobs |> Map.containsKey (PointName nj.name) then
        failwithf "Job \"%O\" has already been registered." (PointName nj.name)

      do! Job.start (supervise instance nj)
      return! f (Map.add (PointName nj.name) 0u jobs)

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
        do! Message.errorf "Job \"%O\" has crashed too many times and won't be restarted." name |> log
        do! IVar.fill shouldRestart false
        return! f jobs

      | _ ->
        do! Message.errorf "Job \"%O\" has crashed and will be restarted." name |> log
        do! IVar.fill shouldRestart true
        return! f (Map.add name (restarted + 1u) jobs)
  }

  f Map.empty

let create options =
  { options = options
    ch      = Ch.Now.create () }

let start instance =
  Job.Global.start (loop instance)
