namespace Logary

open System.Threading
open System

[<AutoOpen>]
module DataSlotExtension =
  type IDataSlot with
    member x.push (dataFac: unit -> _) =
      x.push (lazy (dataFac () |> box))

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module DataSlot =

  type internal AsyncLocalDataSlot () =
    let _currentData = new AsyncLocal<Lazy<obj> list>()
    do _currentData.Value <- list.Empty
   
    let getCurrentDataFromAsyncLocal () =
      let current = _currentData.Value
      if obj.ReferenceEquals(current, null) then
        _currentData.Value <- []
        []
      else current

    let mutable innerDataSlot: IDataSlot option = None

    interface IDataSlot with
      member x.collect () =
        let innerData =
          match innerDataSlot with
          | Some inner -> inner.collect ()
          | None -> []

        getCurrentDataFromAsyncLocal ()
        |> List.map (fun lz -> lz.Value) 
        |> List.append innerData

      member x.push data =
        match innerDataSlot with
        | Some inner ->
          inner.push data
        | None ->
          let previous = getCurrentDataFromAsyncLocal ()
          _currentData.Value <- data :: previous

          {
            new IDisposable with
              member x.Dispose () =
                _currentData.Value <- previous
          }
      
      member x.wrap inner = innerDataSlot <- Some inner

  let useDefault () : IDataSlot  = new AsyncLocalDataSlot () :> IDataSlot
