[<AutoOpen>]
module internal Logary.Adapters.AspNetCore.SystemEx

open System
open System.Runtime.ExceptionServices

type Exception with
  member x.reraise () =
    ExceptionDispatchInfo.Capture(x).Throw()
    Unchecked.defaultof<_>