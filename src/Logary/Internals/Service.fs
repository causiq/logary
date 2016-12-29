namespace Logary.Internals

open NodaTime
open Hopac
open Hopac.Infixes
open Logary
open Logary.Internals
open Logary.Message

type ServiceState =
  | Initialising
  | Running
  | Paused
  | ShuttingDown
  | Shutdown
  | Faulted of unhandled:exn // the supervised job faulted

/// A service is an abstract interface that encapsulates a service
/// implementation.
type Service<'t> =
  abstract name : string
  abstract instance : 't

type InitialisingService<'t> =
  inherit Service<'t>

and RunningService<'t> =
  inherit Service<'t>

and PausedService<'t> =
  inherit Service<'t>

and ShutdownService =
  /// Gets the list of service states and the duration that the service was in
  /// each of them.
  abstract transitions : (ServiceState * Duration) list

module Service =

  type T =
    private {
      name : PointName
      startIV : IVar<unit>
      pauseCh : Ch<IVar<unit> * Promise<unit>>
      resumeCh : Ch<IVar<unit> * Promise<unit>>
      shutdownCh : Ch<IVar<unit>>
      getStateCh : Ch<IVar<ServiceState>>
      serverLoop : Promise<Choice<unit, exn>>
      ilogger : Logger
    }
  with
    override x.ToString() =
      sprintf "Service(%O)" x.name

  let start (t : InitialisingService<T>) : Job<RunningService<T>> =
    t.instance.startIV *<= () >>-. 
    { new RunningService<T> with
        member x.name = t.name
        member x.instance = t.instance }

  let pause (t : RunningService<T>) : Alt<PausedService<T>> =
    Unchecked.defaultof<Alt<PausedService<T>>>

  let resume (t : PausedService<T>) : Alt<RunningService<T>> =
    Unchecked.defaultof<Alt<RunningService<T>>>

  let shutdown (t : #Service<T>) : Alt<ShutdownService> =
    Unchecked.defaultof<Alt<ShutdownService>>

  let getState (t : #Service<T>) : Alt<ServiceState> =
    let hasExited =
      t.instance.serverLoop ^->
      (Choice.bimap (fun () -> Shutdown) Faulted >> Choice.get)

    t.instance.getStateCh *<+=>- id <|> hasExited

  /// Create a new service from a supervised job. It's up to the caller to
  /// decide what sort of policy the supervised job should have.

  let create (ilogger : Logger)
             name pauseCh resumeCh shutdownCh
             (server : SupervisedJob<unit>)
             : Job<InitialisingService<T>> =
    let ilogger = ilogger |> Logger.apply (setSimpleName (sprintf "Logary.Service(%s)" name))
    let pn = PointName.ofSingle name
    let t =
      { name = pn
        startIV = IVar ()
        pauseCh = pauseCh
        resumeCh = resumeCh
        shutdownCh = shutdownCh
        getStateCh = Ch ()
        serverLoop = memo server
        ilogger = ilogger }

    Job.start (t.startIV >>-. t.serverLoop >>-. ()) >>-.
    { new InitialisingService<T> with
        member x.instance = t
        member x.name = name
    }

  let createSimple ilogger name shutdownCh server =
    let noopAcker () =
      let ch = Ch ()
      Job.foreverServer (ch ^=> fun (ack, nack : Promise<unit>) -> IVar.fill ack ())
      >>-. ch

    noopAcker () >>= fun pauseCh ->
    noopAcker () >>= fun resumeCh ->
    create ilogger name pauseCh resumeCh shutdownCh server