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

type Service =
  abstract name : string

/// A service is an abstract interface that encapsulates a service
/// implementation.
type Service<'i> =
  inherit Service
  abstract instance : 'i

and ShutdownService =
  /// Gets the list of service states and the duration that the service was in
  /// each of them.
  abstract transitions : (ServiceState * Duration) list

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Service =

  type T =
    private {
      name : PointName
      pauseCh : Ch<Ch<unit> * Promise<unit>>
      resumeCh : Ch<Ch<unit> * Promise<unit>>
      shutdown : Alt<Promise<unit>>
      getStateCh : Ch<IVar<ServiceState>>
      ilogger : Logger
    }
  with
    override x.ToString() =
      sprintf "Service(%O)" x.name

  let pause (x : Service<T>) : Alt<unit> =
    x.instance.pauseCh *<+->- fun ack nack -> ack, nack

  let resume (x : Service<T>) : Alt<unit> =
    x.instance.resumeCh *<+->- fun ack nack -> ack, nack

  let shutdown (x : Service<T>) : Alt<Promise<unit>> =
    x.instance.shutdown

  let getState (t : Service<T>) : Alt<ServiceState> =
    t.instance.getStateCh *<+=>- id

  /// Create a new service from a supervised job. It's up to the caller to
  /// decide what sort of policy the supervised job should have.

  let create (ilogger : Logger)
             name
             (pause : Alt<_>)
             (resume : Alt<_>)
             (shutdown : Alt<_>)
             (server : SupervisedJob<unit>)
             : Job<Service<T>> =
    let ilogger = ilogger |> Logger.apply (setSimpleName (sprintf "Logary.Service(%s)" name))
    let pn = PointName.ofSingle name
    let mserver = memo server
    let pauseCh, resumeCh, getStateCh = Ch (), Ch (), Ch ()

    let rec running transitions =
      Alt.choose [
        pauseCh ^=>. pause
        resumeCh ^=>. resume
        getStateCh ^=> fun replCh -> mserver ^=> check
      ]
    and check = function
      | Choice1Of2 () ->
        halted ()
      | Choice2Of2 e ->
        faulted e

    and halted () =
      Alt.choose [ ]

    and faulted e =
      Alt.choose []

    running [] >>-.
    { new Service<T> with
        member x.instance =
          { name = pn
            pauseCh = pauseCh
            resumeCh = resumeCh
            shutdown = shutdown
            getStateCh = getStateCh
            ilogger    = ilogger }

        member x.name = name
    }

  let createSimple ilogger name shutdownCh server =
    let noopAcker () : Job<Alt<_>> =
      let ch = Ch ()
      Job.foreverServer (
        ch ^=> fun (ack, nack : Promise<unit>) -> Ch.give ack () <|> nack
      )
      >>-. (ch *<+->- fun ack nack -> ack, nack)

    noopAcker () >>= fun pauseCh ->
    noopAcker () >>= fun resumeCh ->
    create ilogger name pauseCh resumeCh shutdownCh server