namespace Logary

open System
open System.Text.RegularExpressions
open Logary
open Logary.Internals

/// This is the accept filter that is before the message is passed to the logger
type MessageFilter = Message -> bool

/// A rule specifies what messages a target should accept.
[<CustomEquality; CustomComparison>]
type Rule =
  { /// This is the regular expression that the 'path' must match to be loggable
    path     : Regex
    /// This is the level at which the target will accept log lines. It's inclusive, so
    /// anything below won't be accepted.
    minLevel : LogLevel
    /// This is the accept filter that is before the message is passed to the logger
    /// instance.
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
///
/// Rules compose to the maximal filter. I.e. if you have one rule for your
/// target that specifies all Message-s must be of Debug level, and then another
/// filter that specifies they have to be of Info level, then the rule of Info
/// will win, and all Debug-level messages will be filtered from the stream.
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Rule =
  let private allPaths = Regex(".*", RegexOptions.Compiled)

  let empty =
    { path     = allPaths
      acceptIf = fun _ -> true
      minLevel    = Verbose }

  /// Sets the hiera regex field.
  [<CompiledName "SetHiera">]
  let setPath (regex : Regex) (r : Rule) =
    { r with path = regex }

  /// Sets the hiera regex field from a string.
  [<CompiledName "SetHiera">]
  let setPathString (regex : string) (r : Rule) =
    { r with path = Regex(regex) }

  /// Sets the rule's message filter. Remember that if you want to apply multiple
  /// message filters, you need to create a new Rule, or you'll overwrite the
  /// existing message filter.
  [<CompiledName "SetMessageFilter">]
  let setAcceptIf (filter : _ -> _) (r : Rule) =
    { r with acceptIf = filter }

  [<CompiledName "SetLevel">]
  let setMinLevel (minLevel : LogLevel) (r : Rule) =
    { r with minLevel = minLevel }

  /// Create a new rule with the given hiera, target, accept function and min level
  /// acceptable.
  [<CompiledName "Create">]
  let create hiera messageFilter level =
    { path     = hiera
      acceptIf = messageFilter
      minLevel = level }

  /// Create a new rule with the given hiera, target, accept function and min level
  /// acceptable.
  [<CompiledName "Create">]
  let createFunc hiera level (messageFilter : Func<_, _>) =
    { path     = hiera
      acceptIf = fun m -> messageFilter.Invoke m
      minLevel = level }

  // Only used for internal logging so far; maximally permissive for any matching
  // hiera.
  // TODO: consider what this will do for rules of different hierarchies...
  let compile rs =
    let rs = Array.ofSeq rs

    let regex =
      rs
      |> Seq.map (fun r -> r.path.ToString())
      |> String.concat "|"
      |> sprintf "(%s)"

    let rec filter i (rs : _ []) message =
      if i = -1 then false else
      let r = rs.[i]
      if r.acceptIf message then true
      else filter (i - 1) rs message

    { path = Regex(regex, RegexOptions.Compiled)
      acceptIf = if rs.Length = 0 then fun _ -> true else filter (rs.Length) rs
      minLevel = rs |> Seq.map (fun r -> r.minLevel) |> Seq.fold min Fatal
    }

  /// use strict strategy
  let canPass (msg : Message) (rules : Rule list) =
    rules 
    |> List.filter (fun r ->  r.path.IsMatch (PointName.format msg.name))
    |> List.forall (fun r -> msg.level >= r.minLevel && r.acceptIf msg)