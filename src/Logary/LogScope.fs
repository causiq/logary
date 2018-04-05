namespace Logary

open System
open System.Threading

type ILogScope =
  abstract collect: unit -> obj list
  abstract push: Lazy<obj> -> IDisposable
  abstract wrap: ILogScope -> unit

type AsyncLogScope() =
  static let _currentScopeData = new AsyncLocal<Lazy<obj> list>()
  static do _currentScopeData.Value <- list.Empty
 
  let getCurrentScopeDataFromAsyncLocal () =
    let current = _currentScopeData.Value
    if obj.ReferenceEquals(current, null) then
      _currentScopeData.Value <- []
      []
    else current

  let mutable innerScope: ILogScope option = None

  interface ILogScope with
    member x.collect () =
      let innerData =
        match innerScope with
        | Some inner -> inner.collect ()
        | None -> []

      getCurrentScopeDataFromAsyncLocal ()
      |> List.map (fun lz -> lz.Value) 
      |> List.append innerData

    member x.push data =
      match innerScope with
      | Some inner ->
        inner.push data
      | None ->
        let previous = getCurrentScopeDataFromAsyncLocal ()
        _currentScopeData.Value <- data :: previous

        {
          new IDisposable with
            member x.Dispose () =
              _currentScopeData.Value <- previous
        }
    
    member x.wrap inner = innerScope <- Some inner