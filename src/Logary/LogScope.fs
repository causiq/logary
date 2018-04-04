namespace Logary

open System
open System.Threading

/// An log scope with scope name and scope data
type Scope =
  Scope of string * obj

type ILogScope =
  abstract fold: ('state -> Scope -> 'state) -> 'state -> 'state
  abstract push: Scope -> IDisposable

type AsyncLogScope() =
  static let _currentScope = new AsyncLocal<Scope list>()
  static let nullValue = Unchecked.defaultof<Scope list>
  static do _currentScope.Value <- list.Empty
 
  let getCurrentScopeFromAsyncLocal () =
    let current = _currentScope.Value
    if current = nullValue then
      _currentScope.Value <- []
      []
    else current

  let parent = getCurrentScopeFromAsyncLocal ()

  interface ILogScope with
    member x.fold folder state =
      getCurrentScopeFromAsyncLocal ()
      |> List.fold folder state 

    member x.push scope =
      let current = getCurrentScopeFromAsyncLocal ()
      _currentScope.Value <- scope :: current
      x :> IDisposable

  interface IDisposable with
    member x.Dispose () =
      _currentScope.Value <- parent