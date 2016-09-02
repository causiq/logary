module Logary.Supervisor

open System
open Hopac
open Hopac.Infixes
open NodaTime

type Policy =
  | Restart
  | Terminate
  | Delayed of Duration

type MinionInfo =
  {
    name     : PointName
    policy   : Policy
    job      : (obj -> Job<unit>) -> obj option -> Job<unit>
    shutdown : Ch<IVar<unit>>
  }

type private MinionState =
  {
    info : MinionInfo
    state : obj option
  }

type private JobId = private JobId of int

type private Reason =
  | Failure of Exception
  | Complete

type private SupervisorState =
  {
    ident     : int
    minions   : Map<JobId, MinionState>
    processes : Map<JobId, Alt<JobId * Reason>>
    delayed   : Map<PointName, Alt<PointName * (SupervisorState -> Job<SupervisorState>)>>
  }

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
          Map.add jobId (reason ^-> (fun r -> (jobId, r))) state.processes
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
  {
    shutdown   : Ch<IVar<unit>>
    register   : Ch<MinionInfo>
    unregister : Ch<PointName>
  }

let create logger =
  let shutdownCh   = Ch()
  let registerCh   = Ch()
  let unregisterCh = Ch()
  let lastWillCh   = Ch()

  let startMinion minionInfo will state =
    Message.eventVerbose "Starting minion"
    |> Message.setField "name" (PointName.format minionInfo.name)
    |> Logger.log logger
    >>=.
      if SupervisorState.jobNames state |> List.contains minionInfo.name then
        Message.eventVerbose "New minion not started; already supervised"
        |> Message.setField "name" (PointName.format minionInfo.name)
        |> Logger.log logger
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
        Message.eventVerbose "Minion started"
        |> Message.setField "name" (PointName.format minionInfo.name)
        |> Message.setField "jobId" (sprintf "%A" jobId)
        |> Logger.log logger
        >>-. SupervisorState.addMinion jobId minionState reason will state

  let unregisterMinion name state =
    Message.eventVerbose "Unregistering started"
    |> Message.setField "name" (PointName.format name)
    |> Logger.log logger
    >>=.
      match SupervisorState.jobState state name with
      | Some (jobId, minionState) ->
        minionState.info.shutdown *<-=>- id
        >>-. SupervisorState.removeMinion jobId state
      | None ->
        Message.eventWarn "Received request to unregister unknown job"
        |> Message.setField "name" (PointName.format name)
        |> Logger.log logger
        >>-. state

  let handlePolicy jobId minionState state =
    match minionState.info.policy with
    | Terminate ->
      Message.eventDebug "Failure policy: Terminate. Removing from supervision."
      |> Message.setField "name" (PointName.format minionState.info.name)
      |> Logger.log logger
      >>-. SupervisorState.removeMinion jobId state
    | Restart ->
      Message.eventDebug "Failure policy: Restart. Restarting."
      |> Message.setField "name" (PointName.format minionState.info.name)
      |> Logger.log logger
      >>-. SupervisorState.removeMinion jobId state
      >>= startMinion minionState.info minionState.state
    | Delayed delay ->
      let promise =
        timeOut <| delay.ToTimeSpan()
        >>-. (minionState.info.name, startMinion minionState.info minionState.state)
        |> memo
      Message.eventDebug "Failure policy: Delayed. Restarting after delay."
      |> Message.setField "name" (PointName.format minionState.info.name)
      |> Message.setField "delay" (delay.ToString())
      |> Logger.log logger
      >>-. (state
            |> SupervisorState.removeMinion jobId
            |> SupervisorState.addDelayed minionState.info.name (Promise.read promise))

  let handleTermination state jobId reason =
    let minionState = Map.find jobId state.minions
    match reason with
    | Complete ->
      Message.eventDebug "Minion exited without exception, removing from supervision"
      |> Message.setField "name" (PointName.format minionState.info.name)
      |> Logger.log logger
      >>-. SupervisorState.removeMinion jobId state
    | Failure e ->
      Message.eventError "Minion failed"
      |> Message.addExn e
      |> Logger.log logger
      >>=. handlePolicy jobId minionState state

  let replaceLastWill state (jobId, will) =
    Message.eventVerbose "New will received"
    |> Message.setField "jobId" (sprintf "%A" jobId)
    |> Logger.log logger
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
            >>=. Logger.log logger (Message.eventDebug "All minions shutdown")
            |> memo
          Message.eventVerbose "Shutting down minions!"
          |> Logger.log logger
          >>=.
            Alt.choose [
              shutdownAll |> Promise.read
              timeOutMillis 2000
              |> Alt.afterJob
                  (fun () ->
                     Message.eventError "Not all supervised minions shutdown cleanly"
                     |> Logger.log logger)
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

  {
    shutdown   = shutdownCh
    register   = registerCh
    unregister = unregisterCh
  }
