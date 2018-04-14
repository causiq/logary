namespace Logary

open System

type DataSlot =
  abstract collect: unit -> obj list
  abstract push: Lazy<obj> -> IDisposable
  abstract wrap: DataSlot -> unit
