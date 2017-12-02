namespace Logary.EventsProcessing

open Hopac
open Hopac.Infixes
open Logary.EventsProcessing.Transformers

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
type Ticker<'state,'t,'r> (initialState:'state) =
  let tickCh = Ch<unit> ()

  abstract member Folder     : 'state -> 't -> 'state
  abstract member HandleTick : 'state -> 'state * 'r

  member this.InitialState = initialState
  member this.Ticked = tickCh :> Alt<_>
  member this.Tick () = tickCh *<- ()

  member this.TickEvery timespan =
    let cancellation = Cancellation.create ()
    let rec loop () =
      Alt.choose [
        timeOut timespan ^=> fun _ ->
          this.Tick () ^=> fun _ ->
          loop ()

        Cancellation.isCancelled cancellation
      ]

    // TODO: Job.supervise internalLogger (Policy.restart) (loop ()) |> Job.start
    loop () |> Job.start
    >>-. cancellation

module Ticker =
  let create (initialState: 's) (folder: 's -> 't -> 's) (handleTick: 's -> 's * 'r) =
    {
      new Ticker<'s,'t,'r> (initialState) with
        member this.Folder state item = folder state item
        member this.HandleTick state = handleTick state
    }

type BufferTicker<'t> () =
  inherit Ticker<ResizeArray<'t>,'t,ResizeArray<'t>>(ResizeArray())
    override this.Folder state item =
      state.Add item
      state

    override this.HandleTick state =
      ResizeArray(),state

type EWMATicker (rateUnit, alphaPeriod, iclock) =
  inherit Ticker<ExpWeightedMovAvg.EWMAState,int64,float>(ExpWeightedMovAvg.create alphaPeriod iclock)
    override this.Folder ewma item =
      ExpWeightedMovAvg.update ewma item

    override this.HandleTick ewma =
      let ewma' = ExpWeightedMovAvg.tick ewma
      let rate = ewma' |> ExpWeightedMovAvg.rateInUnit rateUnit
      ewma', rate
