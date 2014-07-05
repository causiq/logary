/// API for writing logs and metrics.
/// For gauges that run continuously based on a timer, have a look at
/// the Registry module. To get a logger, have a look at the Logger module.
///
/// If you are using I recommend doing:
/// `Install-Package Intelliplan.Logary.CSharp` instead of dealing with the
/// interop problems that you will get from using this module directly.
module Logary.Log
