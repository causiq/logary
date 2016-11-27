module Logary.Supervisor

open System
open Hopac
open Hopac.Infixes
open NodaTime
open Logary
open Logary.Message

type Policy =
  | Restart
  | Terminate
  | Delayed of Duration

type MinionInfo =
  { name     : PointName
    policy   : Policy
    job      : (obj -> Job<unit>) -> obj option -> Job<unit>
    shutdown : Ch<IVar<unit>> }

type private MinionState =
  { info : MinionInfo
    state : obj option }

type private JobId =
  | JobId of int
  override x.ToString () =
    let (JobId i) = x
    sprintf "JobId %d" i

type private Reason =
  | Failure of Exception
  | Complete

type private SupervisorState =
  { ident     : int
    minions   : Map<JobId, MinionState>
    processes : Map<JobId, Alt<JobId * Reason>>
    delayed   : Map<PointName, Alt<PointName * (SupervisorState -> Job<SupervisorState>)>> }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module private SupervisorState =
  let removeMinion jobId state =
    { state with
        processes = Map.remove jobId state.processes
        minions   = Map.remove jobId state.minions }

  let addMinion jobId minionState (reason : Alt<Reason>) will state =
    { state with
        ident = state.ident + 1
        processes =
          Map.add jobId (reason ^-> (fun r -> jobId, r)) state.processes
        minions   =
          Map.add jobId minionState state.minions }

  let addDelayed name promise state =
    { state with
        delayed = Map.add name promise state.delayed }

  let removeDelayed name state =
    { state with
        delayed = Map.remove name state.delayed }

  let updateWill jobId will state =
    { state with
         minions = Map.map (fun k v -> if k = jobId then { v with state = Some will } else v) state.minions }

  let jobNames state =
    let running =
      state.minions
      |> Map.toList
      |> List.map (fun (_, { info = { name = n } }) -> n)
    let delayed =
      state.delayed
      |> Map.toList
      |> List.map fst
    List.concat [running;delayed]

  let jobState state name =
    state.minions
    |> Map.toSeq
    |> Seq.tryFind (fun (jobId, minionState) -> minionState.info.name = name)

type Instance =
  { shutdown   : Ch<IVar<unit>>
    register   : Ch<MinionInfo>
    unregister : Ch<PointName> }

let create (logger : Logger) =
  let shutdownCh   = Ch()
  let registerCh   = Ch()
  let unregisterCh = Ch()
  let lastWillCh   = Ch()

  let startMinion minionInfo will state =
    logger.verboseBP (
      eventX "Starting minion"
      >> setField "name" (PointName.format minionInfo.name))
    >>=.
      if SupervisorState.jobNames state |> List.contains minionInfo.name then
        logger.verboseBP (
          eventX "New minion not started; already supervised"
          >> setField "name" (PointName.format minionInfo.name))
        >>-. state
      else
        let jobId = JobId state.ident
        let minionState = { info = minionInfo; state = will }
        let work =
          minionInfo.job (fun o -> Ch.give lastWillCh (jobId, o) :> Job<_>) will
        let reason = IVar()
        let guarded =
          Job.tryIn work (fun () -> Job.result Complete) (fun e -> Job.result (Failure e))
          >>= (fun r -> reason *<= r)
        guarded |> start
        logger.verboseBP (
          eventX "Minion '{name}' started as job id {jobId}."
          >> setField "name" (PointName.format minionInfo.name)
          >> setField "jobId" (jobId.ToString()))
        >>-. SupervisorState.addMinion jobId minionState reason will state

  let unregisterMinion name state =
    logger.verboseBP (
      eventX "Unregistering of minion '{name}' started."
      >> setField "name" (PointName.format name))
    >>=.
      match SupervisorState.jobState state name with
      | Some (jobId, minionState) ->
        minionState.info.shutdown *<-=>- id
        >>-. SupervisorState.removeMinion jobId state
        >>-. SupervisorState.removeDelayed name state
      | None ->
        logger.warnBP (
          eventX "Received request to unregister unknown minion named '{name}'."
          >> setField "name" (PointName.format name))
        >>-. state

  let handlePolicy jobId minionState state =
    match minionState.info.policy with
    | Terminate ->
      logger.debugBP (
        eventX "Failure policy: Terminate. Removing from supervision."
        >> setField "name" (PointName.format minionState.info.name))
      >>-. SupervisorState.removeMinion jobId state
    | Restart ->
      logger.debugBP (
        eventX "Failure policy: Restart. Restarting."
        >> Message.setField "name" (PointName.format minionState.info.name))
      >>-. SupervisorState.removeMinion jobId state
      >>= startMinion minionState.info minionState.state
    | Delayed delay ->
      let promise =
        timeOut <| delay.ToTimeSpan()
        >>-. (minionState.info.name, startMinion minionState.info minionState.state)
        |> memo
      logger.debugBP (
        eventX "Failure policy: Delayed. Restarting after delay."
        >> setField "name" (PointName.format minionState.info.name)
        >> setField "delay" (delay.ToString()))
      >>-. (state
            |> SupervisorState.removeMinion jobId
            |> SupervisorState.addDelayed minionState.info.name (Promise.read promise))

  let handleTermination state jobId reason =
    let minionState = Map.find jobId state.minions
    match reason with
    | Complete ->
      logger.debugBP (
        eventX "Minion exited without exception, removing from supervision"
        >> setField "name" (PointName.format minionState.info.name))
      >>-. SupervisorState.removeMinion jobId state
    | Failure e ->
      logger.errorBP (eventX "Minion failed" >> Message.addExn e)
      >>=. handlePolicy jobId minionState state

  let replaceLastWill state (jobId, will) =
    logger.verboseBP (eventX "New will received" >> setField "jobId" (jobId.ToString()))
    >>-. SupervisorState.updateWill jobId will state

  let rec loop state =
    Alt.choose [
      // shutdown
      shutdownCh ^=>
        fun ack ->
          let shutdownMinion (minionState : MinionState) =
            minionState.info.shutdown *<-=>- id
          let shutdownAll =
            Job.seqIgnore (state.minions |> Map.toSeq |> Seq.map (snd >> shutdownMinion))
            >>=. logger.debugBP (eventX "All minions shutdown")
            |> memo
          logger.verboseBP (eventX "Shutting down minions!")
          >>=.
            Alt.choose [
              shutdownAll |> Promise.read
              timeOutMillis 2000
              |> Alt.afterJob (fun () -> logger.errorBP (eventX "Not all supervised minions shutdown cleanly"))
            ] >>=. ack *<= ()

      // anything else will create a new state and then recurse into the loop
      Alt.choose [
        // process delayed restarts
        state.delayed
        |> Map.toSeq
        |> Seq.map snd
        |> Alt.choose
        |> Alt.afterJob
            (fun (delayName, restart) ->
              state
              |> SupervisorState.removeDelayed delayName
              |> restart)

        // register new minion
        registerCh ^=>
          fun minionInfo ->
            startMinion minionInfo None state

        // unregister minion
        unregisterCh ^=>
          fun name -> unregisterMinion name state

        // new last will
        lastWillCh ^=> replaceLastWill state

        // handle termination
        state.processes
        |> Map.toSeq
        |> Seq.map snd
        |> Alt.choose
        |> Alt.afterJob (fun (jid, reason) -> handleTermination state jid reason)
      ] |> Alt.afterJob loop
    ]

  loop { ident = 0; minions = Map.empty; processes = Map.empty; delayed = Map.empty }
  |> start

  { shutdown   = shutdownCh
    register   = registerCh
    unregister = unregisterCh }