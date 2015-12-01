namespace Logary.Supervisor

open System
open Hopac

open Logary
open Logary.Internals
open Logary.DataModel

module Supervisor =

  type internal SupervisorState = Map<string, int>

  type SupervisorMessage =
  /// Register the given job to be supervised.
  /// The job will be automatically restarted if it crashes.
  /// A job can only be registered once.
  | Register of NamedJob<unit>
  /// Removes a job from the supervision list.
  /// Used by the supervisor loop to signal that the job has finished without fault.
  | Unregister of string
  /// Used by a supervisor job to report crashes.
  /// The supervisor sets the IVar to indicate if the job should be restarted.
  | JobErrored of name: string * exn: exn option * shouldRestart: IVar<bool>

  type Options =
    { logger: Logger
      maxRestarts: int
      restartDelay: TimeSpan }
    with

    static member create(logger: Logger, ?maxRestarts: int, ?restartDelay: TimeSpan) =
      { logger = logger
        maxRestarts = defaultArg maxRestarts 10
        restartDelay = defaultArg restartDelay <| TimeSpan.FromMilliseconds 500.0 }

  type Instance =
    { options: Options
      ch:      Ch<SupervisorMessage> }

  let private log instance = Message.Context.serviceSet "Logary.Supervisor" >> instance.options.logger.Log

  /// Register the given job to be supervised.
  /// The job will be automatically restarted if it crashes.
  /// A job can only be registered once.
  let register instace j = job {
    do! Ch.give instace.ch (Register j)
  }

  /// Supervision loop for a single job.
  let rec private supervise instance (nj : NamedJob<_>) = job {
    // Block until the job finishes or crashes.
    let! result = Job.catch nj.job
    let exn = match result with | Choice1Of2 _ -> None | Choice2Of2 e -> Some e

    // If the job is finished without an exception (presumably shut down),
    // stop supervising.
    if exn.IsNone then
      return ()
    else
      // Ask the supervisor if the job should be restarted
      let! shouldRebootVar = IVar.create ()
      do! Ch.give instance.ch (JobErrored (nj.name, exn, shouldRebootVar))
      let! shouldReboot = IVar.read shouldRebootVar

      if shouldReboot then
        do! timeOut instance.options.restartDelay
        return! supervise instance nj
      else
        do! Ch.give instance.ch (Unregister nj.name)
        return ()
  }

  /// Main supervisor loop.
  let private loop (instance: Instance) =
    let log = log instance
    let rec f (jobs: SupervisorState) = job {
      let! msg = Ch.take instance.ch

      match msg with
      | Register nj ->
        // A job can only be registered once.
        if Map.containsKey nj.name jobs then
          failwithf "Job \"%s\" has already been registered." nj.name

        do! Job.start (supervise instance nj)
        return! f (Map.add nj.name 0 jobs)
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
        | (r, mr) when r > mr ->
          log (Message.errorf "Job \"%s\" has crashed too many times and won't be restarted." name)
          do! IVar.fill shouldRestart false
          return! f jobs
        | _ ->
          log (Message.errorf "Job \"%s\" has crashed and will be restarted." name)
          do! IVar.fill shouldRestart true
          return! f (Map.add name (restarted + 1) jobs)
    }

    f Map.empty

  let create options =
    { options = options
      ch      = Ch.Now.create () }

  let start instance =
    Job.Global.start (loop instance)
