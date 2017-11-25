namespace NLog.Targets

open System
open System.Collections.Concurrent
open NodaTime
open Logary
open Logary.Message
open Logary.Internals

/// The functions of the NLog domain and logary codomain.
module internal Adaptation =
  open NLog

  /// Map the NLog.LogLevel to a Logary.LogLevel.
  let mapLogLevel (level : NLog.LogLevel) =
    if level.Equals(NLog.LogLevel.Debug) then
      Debug
    elif level.Equals(NLog.LogLevel.Trace) then
      Verbose
    elif level.Equals(NLog.LogLevel.Error) then
      Error
    elif level.Equals(NLog.LogLevel.Fatal) then
      Fatal
    elif level.Equals(NLog.LogLevel.Info) then
      Info
    elif level.Equals(NLog.LogLevel.Warn) then
      Warn
    else
      Debug

  /// Copied from Logary.Internals.Cache, because disposing this target should
  /// also remove the references to the loggers, whilst Logary.Internals.Cache
  /// would have a static/global cache (more useful for type resolution that
  /// won't change across instances).
  let memoizeFactory<'input, 'output> (cache : ConcurrentDictionary<'input, 'output>) =
    fun (f : 'input -> 'output) ->
      fun x -> cache.GetOrAdd(x, f)

open Adaptation

/// A Logary target for NLog. Uses Logger.logSimple. Takes either a reference to
/// LogManager or a property `Logary` that can be set to give this target the
/// instance of Logary to use for requesting loggers.
[<Target("Logary")>] 
type LogaryTarget(logary : LogManager) =
  inherit Target()

  let memoize = memoizeFactory (ConcurrentDictionary<_, _>())

  let mutable instance = logary

  let getLogger =
    memoize (fun name ->
      if Unchecked.defaultof<LogManager> = instance then
        invalidOp "Please set the Logary property"
      else
        instance.getLogger name)

  new() = new LogaryTarget(Unchecked.defaultof<LogManager>)

  /// Set this property to configure the target. If you're reconfiguring
  /// the target, it's important to note that loggers already resolved won't be
  /// resolved again.
  member x.Logary with get() = instance
                   and set v = instance <- v

  override x.Write (evt : NLog.LogEventInfo) =
    let name = PointName.parse evt.LoggerName
    let logger = getLogger name
    let ex = if isNull evt.Exception then None else Some evt.Exception
    let level = mapLogLevel evt.Level
    let msg = Message.eventFormat (level, evt.Message, evt.Parameters)
    // noteworthy: logger.Log(LEI.Create(..., "hi")), will have Message={0} which will be templated
    // on printing (useless way to do it IMO).
    //printfn "messsage=%s" evt.Message
    { msg with name = name }
    |> Message.setContext "LogEventInfo.Properties" evt.Properties
    |> (ex |> Option.fold (fun s -> addExn) id)
    |> setUTCTicks (evt.TimeStamp.ToUniversalTime()).Ticks
    |> Logger.logSimple logger