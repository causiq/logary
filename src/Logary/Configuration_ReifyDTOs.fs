namespace Logary.Configuration

open Logary.Configuration
open Logary.Configuration.DTOs

[<AutoOpen>]
module ReifyDTOs =

  module internal Impl =
    ()

  let reify (conf : ConfDto) =
    confLogary conf.serviceName