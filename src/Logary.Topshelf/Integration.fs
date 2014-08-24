namespace Topshelf

open System.Runtime.CompilerServices

[<Extension>]
module LogaryConfiguratorExtensions =
  open Topshelf.HostConfigurators
  open Topshelf.Logging

  open Logary
  open Logary.Topshelf

  [<Extension; CompiledName "UseLogary">]
  let useLogary (_ : HostConfigurator) (logary : LogManager) =
    let ilog = logary.GetLogger "Topshelf"
    HostLogger.UseLogger
      { new HostLoggerConfigurator with
          member x.CreateLogWriterFactory () =
            { new LogWriterFactory with
                member x.Get name =
                  LogaryLogWriter(logary.GetLogger name) :> LogWriter
                member x.Shutdown () =
                  LogLine.verbose "LogWriterFactory shutdown called, but Logary integration doesn't support shutting down logary through TopShelf, as it's initialised outside."
                  |> Logger.log ilog
                  () } }