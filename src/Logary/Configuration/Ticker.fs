namespace Logary.Configuration

open Hopac
open Hopac.Infixes
open NodaTime
open Logary
open Logary.Internals
open Logary.Configuration.Transformers

type Cancellation = internal { cancelled: IVar<unit> }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Cancellation =
  let create () =
    { cancelled = IVar () }

  let isCancelled cancellation =
    IVar.read cancellation.cancelled

  let cancel cancellation =
    IVar.tryFill cancellation.cancelled ()

[<AbstractClass>]
type Ticker<'state,'t,'r>(initialState:'state) =
  let tickCh = Ch<unit> ()

  abstract reducer   : state: 'state -> 't -> 'state
  abstract handleTick: 'state -> 'state * 'r

  member _.initialState = initialState
  member _.ticked = tickCh :> Alt<_>
  member _.tick () = tickCh *<- ()

  member x.tickEvery(ri: RuntimeInfo, duration: Duration) =
    let cancellation = Cancellation.create ()

    let rec loop () =
      Alt.choose [
        timeOut (duration.toTimeSpanSafe()) ^=> fun _ ->
          x.tick() ^=> loop

        Cancellation.isCancelled cancellation
      ]

    let xA = loop ()
    Job.start (Job.superviseIgnore ri.logger Policy.exponentialBackoffForever xA)
    >>-. cancellation

module Ticker =
  let create (initialState: 's) (reducer: 's -> 't -> 's) (handleTick: 's -> 's * 'r) =
    { new Ticker<'s,'t,'r>(initialState) with
        member x.reducer state item = reducer state item
        member x.handleTick state = handleTick state
    }

type BufferTicker<'t> () =
  inherit Ticker<ResizeArray<'t>, 't, list<'t>>(ResizeArray())
    override _.reducer state item =
      state.Add item
      state

    override _.handleTick state =
      ResizeArray(), state |> List.ofSeq

type EWMATicker (rateUnit, alphaPeriod, iclock) =
  inherit Ticker<ExpWeightedMovAvg.EWMAState,int64,float>(ExpWeightedMovAvg.create alphaPeriod iclock)
    override _.reducer ewma item =
      ExpWeightedMovAvg.update ewma item

    override _.handleTick ewma =
      let ewma' = ExpWeightedMovAvg.tick ewma
      let rate = ewma' |> ExpWeightedMovAvg.rateInUnit rateUnit
      ewma', rate
