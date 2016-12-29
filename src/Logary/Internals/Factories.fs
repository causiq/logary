namespace Logary.Internals

/// This is useful to implement if you want add-on assemblies to be able to
/// extend your builder. Then you just implement an interface that also
/// inherits this interface and makes your extensions to the target configuration
/// be extension methods in the extending assembly, calling this method to
/// read the conf, and then returning the modified configuration to the
/// builder by other means (e.g. by calling a method on the 'intermediate')
/// interface (that is in the core target builder configuration). Since the
/// builder knows its callback, it can implement this 'intermediate' interface
/// with a method taking the new configuration (that was read from and mutated
/// from here).
type ConfigReader<'a> =
  /// an accessor for the internal state; don't use unless you know what you're
  /// doing! Used by the migrations to get the current configuration. Allows you
  /// to modify or use the configuration.
  abstract ReadConf : unit -> 'a