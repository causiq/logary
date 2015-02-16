namespace Logary.Topshelf

open System

open Logary
open global.Topshelf.Logging

[<Obsolete("Use TopshelfAdapter")>]
type LogaryLogWriter(logger : Logger) =
  inherit global.Topshelf.Logging.TopshelfAdapter(logger)  