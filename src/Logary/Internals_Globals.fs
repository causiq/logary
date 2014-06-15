namespace Logary.Internals

/// Module that is the ONLY module allowed to have global
/// variables; created so that consumer applications may
/// call into Logary without having a reference to the LogManager
/// first.
/// Also used to have a method for handling internal logging (the
/// chicken and the egg problem, but I'm looking to inject the
/// internal logger in a nicer fashion).
/// The globals in this module are configured in module "Logging"
/// and module "InternalLogger".
module internal Globals =
  open Logary

  /// This instance is the locked object for the singleton configuration.
  let criticalSection = obj()

  /// This is the "Global Variable" containing the last configured
  /// Logary instance. If you configure more than one logary instance
  /// this will be replaced. It is internal so that noone
  /// changes it from the outside. Don't use this directly if you can
  /// avoid it, and instead take a c'tor dependency on LogaryRegistry
  /// or use IoC with a contextual lookup to resolve proper loggers.
  let singleton : LogaryInstance option ref = ref None

  /// A list of all loggers yet to be configured
  let flyweights : FlyweightLogger list ref = ref []

  /// The writer to use for printing internal logging. Currently
  /// a global variable, that I would prefer to change into something
  /// that is not global.
  let write : (string -> unit) ref =
    ref (System.Console.WriteLine : string -> unit)
