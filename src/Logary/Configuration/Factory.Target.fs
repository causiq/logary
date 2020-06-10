namespace Logary.Configuration.Target

open System
open System.Text.RegularExpressions
open Logary

/// This interface is used to construct a target specific configuration
/// from the builder.
[<NoEquality; NoComparison>]
type SpecificTargetConf =
  /// Build the target configuration from a name (and previously called
  /// methods on the instance behind the interface).
  abstract Build: string -> TargetConf

/// You cannot supply your own implementation of this interface; its aim is
/// not to provide Liskov substitution, but rather to guide you to use the
/// API properly/easily.
[<NoEquality; NoComparison>]
type TargetConfBuild<'T when 'T :> SpecificTargetConf> =

  /// Target-specific configuration, varies by T
  abstract Target : 'T

  /// The minimum level that the target logs with. Inclusive, so it
  /// will configure the rule for the target to log just above this.
  abstract MinLevel: LogLevel -> TargetConfBuild<'T>

  /// Only log with the target if the source path matches the regex.
  /// You can use (new Regex(".*")) to allow any, or simply avoid calling
  /// this method.
  abstract SourceMatching: Regex -> TargetConfBuild<'T>

  /// <summary>
  /// Only accept log lines that match the acceptor.
  /// </summary>
  /// <param name="acceptor">
  /// The function to call for every log line, to verify
  /// whether to let it through
  /// </param>
  abstract AcceptIf: Func<Model.LogaryMessageBase, bool> -> TargetConfBuild<'T>

/// All SpecificTargetConf implementors should take this as their single argument
/// ctor, to go back into the parent context
type ParentCallback<'T when 'T :> SpecificTargetConf> =
  SpecificTargetConf -> TargetConfBuild<'T> ref