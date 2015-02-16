namespace Logary.Suave

open System

open global.Suave.Logging

[<Obsolete("Use Sauve.Logging.SuaveAdapter")>]
type SuaveAdapter(logger) =
  inherit global.Suave.Logging.SuaveAdapter(logger)