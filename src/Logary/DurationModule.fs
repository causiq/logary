namespace Logary


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Duration =
  open NodaTime
  let hours (dur: Duration) =
    dur.TotalTicks / float NodaConstants.TicksPerHour

  let minutes (dur: Duration) =
    dur.TotalTicks / float NodaConstants.TicksPerMinute

  let seconds (dur: Duration) =
    dur.TotalTicks / float NodaConstants.TicksPerSecond

  let milliseconds (dur: Duration) =
    dur.TotalTicks / float NodaConstants.TicksPerMillisecond

  let microseconds =
    ((*) 1000.) << milliseconds
  let ticks (dur: Duration) =
    dur.TotalTicks
  let nanoseconds =
    ((*) 1000.) << microseconds