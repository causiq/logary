namespace System

open System
open System.Runtime.ExceptionServices

[<AutoOpen>]
module SystemEx =
  type Exception with
    member x.reraise () =
      ExceptionDispatchInfo.Capture(x).Throw()
      Unchecked.defaultof<_>

