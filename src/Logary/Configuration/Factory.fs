/// The FactoryApi is to make it much easier to configure Logary from a language
/// such as C#. It's AutoOpen because opening Logary.Configuration should
/// expose the ConfBuilder type inside this module without any further ado.
/// Besides that, this module doesn't contain much in terms of functionality/functions
/// but lets you configure all of that through interaction with the types/classes/objects.
namespace Logary.Configuration

open System
open System.Reflection
open System.Threading.Tasks
open System.Runtime.CompilerServices
open Hopac
open Hopac.Infixes
open NodaTime
open Logary
open Logary.CSharp
open Logary.Configuration

type internal ConfBuilderTarget<'T when 'T :> Target.SpecificTargetConf> =
  { parent   : ConfBuilder        // logary that is being configured
    tr       : Rule               // for this specific target
    specific : 'T option }
with
  member internal x.setSpecific specConf =
    { x with specific = Some specConf }

  interface Target.TargetConfBuild<'T> with
    member x.MinLevel logLevel =
      { x with tr = { x.tr with Rule.minLevel = logLevel } }
      :> Target.TargetConfBuild<'T>

    member x.SourceMatching regex =
      { x with tr = { x.tr with Rule.path = regex } }
      :> Target.TargetConfBuild<'T>

    member x.AcceptIf acceptor =
      { x with tr = { x.tr with Rule.acceptIf = acceptor.Invoke } }
      :> Target.TargetConfBuild<'T>

    member x.Target = Option.get x.specific

/// The "main" fluent-config-api type with extension method for configuring
/// Logary rules as well as configuring specific targets.
and ConfBuilder(conf) =
  member internal x.conf = conf

  member x.InternalLogger (value : ILogger) : ConfBuilder =
    conf
    |> Config.ilogger value
    |> ConfBuilder

  /// Call this method to add middleware to Logary. Middleware is useful for interrogating
  /// the context that logging is performed in. It can for example ensure all messages
  /// have a context field 'service' that specifies what service the code is running in.
  ///
  /// Please see Logary.Middleware for common middleware to use.
  member x.UseFunc(middleware : Func<Func<Message, Message>, Func<Message, Message>>) : ConfBuilder =
    conf
    |> Config.middleware (fun next msg -> middleware.Invoke(new Func<_,_>(next)).Invoke msg)
    |> ConfBuilder
    
  /// Call this method to add middleware to Logary. Middleware is useful for interrogating
  /// the context that logging is performed in. It can for example ensure all messages
  /// have a context field 'service' that specifies what service the code is running in.
  ///
  /// Please see Logary.Middleware for common middleware to use.
  member x.Use(middleware : Middleware) : ConfBuilder =
    conf
    |> Config.middleware middleware
    |> ConfBuilder

  /// Depending on what the compiler decides; we may be passed a MethodGroup that
  /// can be converted to this signature:
  member x.Use(middleware : Func<Message -> Message, Message, Message>) =
    conf
    |> Config.middleware (fun next msg -> middleware.Invoke(next, msg))
    |> ConfBuilder

  /// Configure a target of the type with a name specified by the parameter name.
  /// The callback, which is the second parameter, lets you configure the target.
  member x.Target<'T when 'T :> Target.SpecificTargetConf>
           (name : string,
            configurator : Func<Target.TargetConfBuild<'T>, Target.TargetConfBuild<'T>>)
          : ConfBuilder =

    let builderType = typeof<'T>

    let container : ConfBuilderTarget<'T> =
      { parent   = x
        tr       = Rule.empty
        specific = None }

    let contRef = ref (container :> Target.TargetConfBuild<_>)

    let parentCc : Target.ParentCallback<_> =
      fun tcSpec ->
        let b = !contRef :?> ConfBuilderTarget<'T>
        contRef := { b with specific = Some (tcSpec :?> 'T) } :> Target.TargetConfBuild<_>
        contRef

    let tcSpecific = Activator.CreateInstance(builderType, parentCc) :?> 'T

    contRef := { container with specific = Some tcSpecific } :> Target.TargetConfBuild<_>

    // escape of the type system to get back to this mutually recursive
    // builder class: hence the comment that the interface TargetConfBuild<_> is not
    // referentially transparent
    let targetConf = configurator.Invoke(!contRef) :?> ConfBuilderTarget<'T>

    //conf
    //|> Config.rule targetConf.tr
    //|> Config.target (targetConf.tcSpecific.Value.Build name)
    //|> ConfBuilder
    failwith "TODO!!"

/// Extensions to make it easier to construct Logary
[<Extension; AutoOpen>]
module FactoryApiExtensions =
  open System
  open Logary
  open Logary.Configuration

  /// <summary>
  /// Configure the target with default settings.
  /// </summary>
  /// <typeparam name="T">The <see cref="TargetConf"/> to configure
  /// with defaults</typeparam>
  /// <param name="builder"></param>
  /// <returns>The same as input</returns>
  [<Extension; CompiledName "Target">]
  let target<'T when 'T :> Target.SpecificTargetConf> (builder : ConfBuilder) (name : string) =
    builder.Target<'T>(name, new Func<_, _>(id))

/// The main entry point for object oriented languages to interface with Logary,
/// to configure it.
type LogaryFactory =
  /// Configure a new Logary instance. This will also give real targets to the flyweight
  /// targets that have been declared statically in your code. If you call this
  /// you get a log manager that you can later dispose, to shutdown all targets.
  static member New(service : string, host : string, configurator : Func<ConfBuilder, ConfBuilder>) : Task<LogManager> =
    if service = null then nullArg "service"
    if configurator = null then nullArg "configurator"
    let config = Config.create service host
    let cb = configurator.Invoke (ConfBuilder config)
    let xJ = Config.build cb.conf |> Job.map Registry.toLogManager
    Job.ToTask xJ