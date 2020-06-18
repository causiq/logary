[<AutoOpen>]
module internal Logary.SystemEx

open System
open System.Runtime.ExceptionServices

type Exception with
  member x.reraise () =
    ExceptionDispatchInfo.Capture(x).Throw()
    Unchecked.defaultof<_>

type MLogLevel = Microsoft.Extensions.Logging.LogLevel
type LLogLevel = Logary.LogLevel

type Microsoft.Extensions.Logging.LogLevel with
  member x.asLogary =
    match x with
    | MLogLevel.None | MLogLevel.Critical -> LLogLevel.Fatal
    | MLogLevel.Error -> LLogLevel.Error
    | MLogLevel.Warning -> LLogLevel.Warn
    | MLogLevel.Information -> LLogLevel.Info
    | MLogLevel.Debug -> LLogLevel.Debug
    | _ | MLogLevel.Trace -> LogLevel.Verbose
