namespace Logary.Configuration.HealthCheck

open Logary
open System
open System.Text.RegularExpressions

[<NoEquality; NoComparison>]
type SpecificHealthCheckConf =
  abstract Build : string -> HealthCheckConf

[<NoEquality; NoComparison>]
type HealthCheckConfBuild<'T when 'T :> SpecificHealthCheckConf> =
  abstract HealthCheck : 'T
  abstract MinLevel : LogLevel -> HealthCheckConfBuild<'T>
  abstract SourceMatching : Regex -> HealthCheckConfBuild<'T>
  abstract AcceptIf : Func<Message, bool> -> HealthCheckConfBuild<'T>

type ParentCallback<'T when 'T :> SpecificHealthCheckConf> =
  SpecificHealthCheckConf -> HealthCheckConfBuild<'T> ref