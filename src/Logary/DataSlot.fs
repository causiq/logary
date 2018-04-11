namespace Logary

open System

type IDataSlot =
  abstract collect: unit -> obj list
  abstract push: Lazy<obj> -> IDisposable
  abstract wrap: IDataSlot -> unit
