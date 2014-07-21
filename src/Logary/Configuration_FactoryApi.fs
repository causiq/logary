/// The FactoryApi is to make it much easier to configure Logary from a language
/// such as C#. It's AutoOpen because opening Logary.Configuration should
/// expose the ConfBuilder type inside this module without any further ado.
/// Besides that, this module doesn't contain much in terms of functionality/functions
/// but lets you configure all of that through interaction with the types/classes/objects.
[<AutoOpen>]
module Logary.Configuration.FactoryApi

open System
open System.Reflection

open Logary
open Logary.Configuration
open Logary.Target
open Logary.Target.FactoryApi

type internal ConfBuilderT<'T when 'T :> SpecificTargetConf> =
  { parent      : ConfBuilder        // logary that is being configured
    tr          : rule               // for this specific target
    tcSpecific  : 'T option }
with
  member internal x.SetTcSpecific tcs =
    { x with tcSpecific = Some tcs }

  interface TargetConfBuild<'T> with

    member x.MinLevel logLevel =
      { x with tr = { x.tr with rule.level = logLevel } }
      :> TargetConfBuild<'T>

    member x.SourceMatching regex =
      { x with tr = { x.tr with rule.hiera = regex } }
      :> TargetConfBuild<'T>

    member x.AcceptIf acceptor =
      { x with tr = { x.tr with rule.lineFilter = acceptor.Invoke } }
      :> TargetConfBuild<'T>

    member x.Target = x.tcSpecific.Value

/// The "main" fluent-config-api type with extension method for configuring
/// Logary rules as well as configuring specific targets.
and ConfBuilder(conf) =
  member internal x.BuildLogary () =
    conf |> Config.validate |> runLogary |> asLogManager

  /// Configure a target of the type with a name specified by the parameter
  /// name
  member x.Target<'T when 'T :> SpecificTargetConf>
    (name : string)
    (f : Func<TargetConfBuild<'T>, TargetConfBuild<'T>>)
    : ConfBuilder =

    let builderType = typeof<'T>

    let container : ConfBuilderT<'T> =
      { parent     = x
        tr         = Rule.forAny name
        tcSpecific = None }

    let contRef = ref (container :> TargetConfBuild<_>)

    let parentCc : ParentCallback<_> =
      fun tcSpec ->
        let b = !contRef :?> ConfBuilderT<'T>
        contRef := { b with tcSpecific = Some(tcSpec :?> 'T) } :> TargetConfBuild<_>
        contRef

    let tcSpecific = Activator.CreateInstance(builderType, parentCc) :?> 'T

    contRef := { container with tcSpecific = Some tcSpecific } :> TargetConfBuild<_>

    // escape of the type system to get back to this mutually recursive
    // builder class: hence the comment that the interface TargetConfBuild<_> is not
    // referentially transparent
    let targetConf = f.Invoke(!contRef) :?> ConfBuilderT<'T>

    ConfBuilder(
      conf
      |> withRule targetConf.tr
      |> withTarget (targetConf.tcSpecific.Value.Build name))

open System.Runtime.CompilerServices

/// Extensions to make it easier to construct Logary
[<Extension; AutoOpen>]
module FactoryApiExtensions =
  open System
  open Logary
  open Logary.Target.FactoryApi
  open Logary.Configuration

  /// <summary>
  /// Configure the target with default settings.
  /// </summary>
  /// <typeparam name="T">The <see cref="TargetConf"/> to configure
  /// with defaults</typeparam>
  /// <param name="builder"></param>
  /// <returns>The same as input</returns>
  [<Extension; CompiledName "Target">]
  let target<'T when 'T :> SpecificTargetConf> (builder : ConfBuilder) (name : string) =
    builder.Target<'T> name (new Func<_, _>(id))

/// The main entry point for object oriented languages to interface with Logary,
/// to configure it.
type LogaryFactory =
  /// Configure a new Logary instance. This will also give real targets to the flyweight
  /// targets that have been declared statically in your code. If you call this
  /// you get a log manager that you can later dispose, to shutdown all targets.
  static member New(serviceName : string, configurator : Func<ConfBuilder, ConfBuilder>) : LogManager =
    if configurator = null then nullArg "configurator"
    let c = Config.confLogary serviceName
    let cb = configurator.Invoke <| ConfBuilder(c)
    cb.BuildLogary ()
