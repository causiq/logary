namespace Topshelf

open System.Runtime.CompilerServices

[<Extension; AutoOpen>]
module LogaryConfiguratorExtensions =
  open Topshelf.HostConfigurators
  open Topshelf.Logging
  open Logary
  open Logary.Adapters.Topshelf

  [<Extension; CompiledName "UseLogary">]
  let useLogary (_: HostConfigurator) (logary: LogManager) =
    let ilog = logary.getLogger (PointName [| "Topshelf" |])

    HostLogger.UseLogger
      { new HostLoggerConfigurator with
          member x.CreateLogWriterFactory () =
            { new LogWriterFactory with
                member x.Get name =
                  let pn = PointName.parse name
                  TopshelfAdapter(logary.getLogger pn)
                  :> LogWriter

                member x.Shutdown () =
                  let message =
                    "LogWriterFactory shutdown called, but Logary integration " +
                    "doesn't support shutting down logary through Topshelf, " +
                    "as it's initialised outside."

                  Message.event Verbose message |> Logger.logSimple ilog
            }
      }