namespace Logary

open System.Runtime.CompilerServices

[<AutoOpen; Extension>]
module LogManagerEx =

  type LogManager with
    member x.getLogger name =
      x.getLogger (PointName.parse name)