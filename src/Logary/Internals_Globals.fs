namespace Logary.Internals

/// Module that is the ONLY module allowed to have global variables; created so
/// that consumer applications may call into Logary without having a reference
/// to the LogManager first.
///
/// The globals in this module are configured in module "Logging".
module internal Globals =
  open Logary

  /// This is the "Global Variable" containing the last configured
  /// Logary instance. If you configure more than one logary instance
  /// this will be replaced. It is internal so that noone
  /// changes it from the outside. Don't use this directly if you can
  /// avoid it, and instead take a c'tor dependency on LogaryRegistry
  /// or use IoC with a contextual lookup to resolve proper loggers.
  let singleton : LogaryInstance option ref = ref None

  /// This is the global console semaphore to use when printing in a multi-
  /// threaded manner to STDOUT or STDERR.
  let consoleSemaphore = obj ()

  /// A list of all loggers yet to be configured
  let private flyweights : FlyweightLogger list ref = ref []

  let private flyweightLock = obj ()

  let addFlyweight (fwl : FlyweightLogger) =
    lock flyweightLock <| fun _ ->
      flyweights := fwl :: !flyweights

  /// Gives f a snapshot of the current flyweights
  let withFlyweights f =
    f !flyweights

  let clearFlywieghts () =
    lock flyweightLock (fun _ -> flyweights := [])