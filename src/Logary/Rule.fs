namespace Logary

open System
open System.Text.RegularExpressions
open Logary
open Logary.Internals

/// This is the accept filter that is before the message is passed to the target
type MessageFilter = Message -> bool

/// A rule specifies what messages a target should accept.
/// we do not encourage use rules heavily,only if when the target itself can decide which msg are acceptable.
/// usually this decision is made by eventProcessing pipeline, can be done with code rather than the rules.
[<CustomEquality; CustomComparison>]
type Rule =
  { /// This is the regular expression that the 'path' must match to be loggable
    path     : Regex
    /// This is the level at which the target will accept log lines. It's inclusive, so
    /// anything below won't be accepted.
    minLevel : LogLevel
    /// This is the accept filter that is before the message is passed to the target instance.
    acceptIf : MessageFilter }

  override x.GetHashCode () =
    hash (x.path.ToString(), x.minLevel)

  override x.Equals other =
    match other with
    | null -> false
    | :? Rule as o -> (x :> IEquatable<Rule>).Equals(o)
    | _ -> false

  interface System.IEquatable<Rule> with
    member x.Equals r =
      r.path.ToString() = x.path.ToString()
      && r.minLevel = x.minLevel

  interface System.IComparable with
    member x.CompareTo yobj =
      match yobj with
      | :? Rule as y ->
        compare (x.path.ToString()) (y.path.ToString())
        |> thenCompare x.minLevel y.minLevel
      | _ -> invalidArg "yobj" "cannot compare values of different types"

  override x.ToString() =
    sprintf "Rule(path=%O, minLevel=%O)" x.path x.minLevel

/// Module for dealing with rules. Rules take care of filtering too verbose
/// log lines and measures before they are sent to the targets.
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Rule =
  let private allPaths = Regex(".*", RegexOptions.Compiled)

  let empty =
    { path = allPaths
      acceptIf = fun _ -> true
      minLevel = Verbose }

  /// Sets the hiera regex field.
  [<CompiledName "SetHiera">]
  let setPath (regex: Regex) (r: Rule) =
    { r with path = regex }

  /// Sets the hiera regex field from a string.
  [<CompiledName "SetHiera">]
  let setPathString (regex: string) (r: Rule) =
    { r with path = Regex regex }

  /// Sets the rule's message filter. Remember that if you want to apply multiple
  /// message filters, you need to create a new Rule, or you'll overwrite the
  /// existing message filter.
  [<CompiledName "SetMessageFilter">]
  let setAcceptIf (filter: _ -> _) (r: Rule) =
    { r with acceptIf = filter }

  [<CompiledName "SetLevel">]
  let setMinLevel (minLevel: LogLevel) (r: Rule) =
    { r with minLevel = minLevel }

  /// Create a new rule with the given hiera, target, accept function and min level
  /// acceptable.
  [<CompiledName "Create">]
  let create hiera messageFilter level =
    { path = hiera
      acceptIf = messageFilter
      minLevel = level }

  /// Create a new rule with the given hiera, target, accept function and min level
  /// acceptable.
  [<CompiledName "Create">]
  let createFunc path level (messageFilter: Func<_, _>) =
    { path = path
      acceptIf = fun m -> messageFilter.Invoke m
      minLevel = level }

  /// use the first matched rule
  let canPass (msg: Message) (rules: Rule list) =
    rules
    |> List.tryFind (fun r -> r.path.IsMatch (PointName.format msg.name))
    |> function
      | Some r ->
        msg.level >= r.minLevel && r.acceptIf msg
      | None ->
        false