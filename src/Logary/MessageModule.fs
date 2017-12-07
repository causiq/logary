namespace Logary

open Hopac
open NodaTime
open System
open System.Threading.Tasks
open System.Diagnostics
open Logary.Internals
open Logary.Internals.Aether
open Logary.Internals.Aether.Operators
open Logary.Internals.FsMessageTemplates
open Logary

/// Open this module to log in a more succinct way.
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Message =

  module Optic =

    let name_ : Lens<Message, PointName> =
      (fun (x : Message) -> x.name),
      fun v (x : Message) -> { x with name = v }

    let value_ : Lens<Message, PointValue> =
      (fun (x : Message) -> x.value),
      fun v (x : Message) -> { x with value = v }

    let context_ : Lens<Message, _> =
      (fun (x : Message) -> x.context),
      (fun v (x : Message) -> { x with context = v })

    let level_ : Lens<Message, LogLevel> =
      (fun (x : Message) -> x.level),
      (fun v (x : Message) -> { x with level = v })

    let timestamp_ : Lens<Message, EpochNanoSeconds> =
      (fun (x : Message) -> x.timestamp),
      (fun v (x : Message) -> { x with timestamp = v })

    let boxWithOption_<'a> : Epimorphism<obj option,'a> =
      (function | Some x -> Some (unbox<'a> x)
                | None -> None),
      box >> Some

    let contextValue_ name =
      context_ >-> HashMap.value_ name >-> boxWithOption_


  //#region CONTEXT AND FIELDS

  ///////////////// CONTEXT ////////////////////

  /// Sets a context value if name exist, value will be override
  [<CompiledName "SetContext">]
  let setContext name value message =
    if isNull name then message
    else Optic.set (Optic.contextValue_ name) value message

  /// Sets a context value.
  [<Obsolete ("Use SetContext instand.")>]
  [<CompiledName "SetContextValue">]
  let setContextValue name value message =
    setContext name value message

  [<CompiledName "SetContextValues">]
  let setContextValues (values : (string * obj) seq) message =
    values |> Seq.fold (fun m (name, value) -> setContext name value m) message

  [<CompiledName "SetContextFromMap">]
  let setContextFromMap (m : Map<string, obj>) message =
    setContextValues (m |> Map.toSeq) message

  /// Uses reflection to set all
  [<CompiledName "SetContextFromObject">]
  let setContextFromObject (data : obj) message =
    Map.ofObject data
    |> HashMap.toSeq
    |> fun values -> setContextValues values message

  /// Tries to get a context value
  [<CompiledName "TryGetContext">]
  let inline tryGetContext name message =
    if isNull name then None
    else Optic.get (Optic.contextValue_ name) message

  [<CompiledName "GetContextsByPrefix">]
  let inline GetContextsByPrefix (prefix : string) message =
    if String.IsNullOrEmpty prefix then
      message.context |> HashMap.toSeq
    else
      let prefixLen = prefix.Length
      message.context
      |> Seq.choose (fun (KeyValue (k,v)) ->
         if k.StartsWith prefix then
           let withoutPrefix = k.Substring(prefixLen)
           Some (withoutPrefix, v)
         else None)

  [<CompiledName "GetContextsOtherThanGaugeAndFields">]
  let inline getContextsOtherThanGaugeAndFields message =
    message.context
    |> Seq.choose (fun (KeyValue (k,v)) ->
       if k.StartsWith KnownLiterals.FieldsPrefix
          || k.StartsWith KnownLiterals.GaugeTypePrefix then None
       else Some (k,v))

  ///////////////// FIELDS ////////////////////

  /// Get a partial setter lens to a field
  [<CompiledName "SetField">]
  let inline setField name value message =
    let ctxFieldsKey = KnownLiterals.FieldsPrefix + name
    setContext ctxFieldsKey value message

  /// Get a partial setter lens to a field with an unit
  /// trans field to gauge
  [<CompiledName "SetFieldUnit">]
  let inline setFieldUnit name value units message =
    setField name (Gauge (value,units)) message

  /// trans field to gauge

  [<Obsolete ("Use setField instead.")>]
  [<CompiledName "SetFieldValue">]
  let setFieldValue (name : string) (field : Field) message =
    setField name field message

  [<Obsolete ("Use SetFieldsFromSeq instead.")>]
  [<CompiledName "SetFieldValues">]
  let setFieldValues (fields : (string * Field) seq) message =
    fields |> Seq.fold (fun m (name, value) -> setFieldValue name value m) message

  [<Obsolete ("Use SetFieldsFromSeq instead.")>]
  [<CompiledName "SetFieldValuesArray">]
  let setFieldValuesArray (fields : (string * Field)[]) message =
    fields |> Array.fold (fun m (name, value) -> setFieldValue name value m) message

  [<CompiledName "SetFieldsFromSeq">]
  let setFieldsFromSeq (fields : (string * obj) seq) message =
    fields
    |> Seq.fold (fun m (name, value) -> setField name value m) message

  [<CompiledName "SetFieldsFromMap">]
  let setFieldsFromMap (m : Map<string, obj>) message =
    m
    |> Map.fold (fun m k v -> setField k v m ) message

  [<CompiledName "SetFieldsFromMap">]
  let setFieldsFromHashMap (m : HashMap<string, obj>) message =
    m
    |> Seq.fold (fun m (KeyValue (k,v)) -> setField k v m ) message

  [<Obsolete ("Use setField instead.")>]
  [<CompiledName "SetFieldFromObject">]
  let setFieldFromObject name (data : obj) message =
    setField name data message

  /// Reflects over the object and sets the appropriate fields.
  [<CompiledName "SetFieldsFromObject">]
  let setFieldsFromObject (data : obj) message =
    Map.ofObject data
    |> flip setFieldsFromHashMap message

  /// Get a partial getter lens to a field
  [<CompiledName "TryGetField">]
  let tryGetField name message =
    let ctxFieldsKey = KnownLiterals.FieldsPrefix + name
    tryGetContext ctxFieldsKey message

  [<CompiledName "GetAllFields">]
  let getAllFields message =
    GetContextsByPrefix KnownLiterals.FieldsPrefix message

  [<CompiledName "GetAllGauges">]
  let getAllGauges message =
    message
    |> GetContextsByPrefix KnownLiterals.GaugeTypePrefix
    |> Seq.choose (function | (k, (:? Gauge as gauge)) -> Some (k, gauge) | _ -> None)

  [<CompiledName "GetAllTags">]
  let getAllTags message =
    match tryGetContext KnownLiterals.TagsContextName message with
    | Some (tags:Set<string>)-> tags
    | _ -> Set.empty

  [<CompiledName "GetAllSinks">]
  let getAllSinks message =
    match tryGetContext KnownLiterals.SinkTargetsContextName message with
    | Some (sinks:Set<string>)-> sinks
    | _ -> Set.empty

  [<CompiledName "GetAllSinks">]
  let addSinks (sinks : string list) message =
    let sinks = message |> getAllSinks |> Set.union (Set.ofList sinks)
    setContext KnownLiterals.SinkTargetsContextName sinks message

  /// Tag the message
  [<CompiledName "Tag">]
  let tag (tag : string) (message : Message) =
    let tags = message |> getAllTags |> Set.add tag
    setContext KnownLiterals.TagsContextName tags message

  /// Check if the Message has a tag
  [<CompiledName "HasTag">]
  let hasTag (tag : string) (message : Message) =
    message |> getAllTags |> Set.contains tag

  //#endregion

  //#region CTORS

  ///////////////// CTORS ////////////////////

  /// Creates a new event message template with level
  [<CompiledName "Event">]
  let event level template =
    { name      = PointName.empty
      value     = Event (string template)
      context   = HashMap.empty
      level     = level
      timestamp = Global.getTimestamp () }

  /// Creates a new event message template with level. Compared to `event`,
  /// this function has its parameters' order flipped.
  [<CompiledName "Event">]
  let eventX template level =
    event level template

  /// one message can take multi gauges
  [<CompiledName "AddGauge">]
  let addGauge gaugeType (gauge : Gauge) message =
    let gaugeTypeName = KnownLiterals.GaugeTypePrefix + gaugeType
    message |> setContext gaugeTypeName gauge


  [<CompiledName "TryGetGauge">]
  let tryGetGauge gaugeType message : Gauge option =
    let gaugeTypeName = KnownLiterals.GaugeTypePrefix + gaugeType
    message |> tryGetContext gaugeTypeName

  /// Creates a new gauge message with gauge
  [<CompiledName "Gauge">]
  let gaugeMessage gaugeType gauge =
    event LogLevel.Debug String.Empty
    |> addGauge gaugeType gauge

  /// Creates a new gauge message with data point name, unit and value
  [<CompiledName "Gauge">]
  let gaugeWithUnit gaugeType value units =
    gaugeMessage gaugeType (Gauge (value, units))

  /// Creates a new gauge message with data point name and scalar value
  [<CompiledName "Gauge">]
  let gauge gaugeType value =
    gaugeMessage gaugeType (Gauge (value, Units.Scalar))

  [<Obsolete ("Use gaugeWithUnit instead.")>]
  [<CompiledName "Derived">]
  let derivedWithUnit dp value units =
    gaugeWithUnit dp value units

  [<Obsolete ("Use gauge instead.")>]
  [<CompiledName "Derived">]
  let derived dp value =
    gauge dp value

  /// Create a verbose event message
  [<CompiledName "EventVerbose">]
  let eventVerbose = event Verbose

  /// Create a verbose event message, for help constructing format string, see:
  /// http://msdn.microsoft.com/en-us/library/vstudio/ee370560.aspx
  /// Warning: Prefer not formatting data into the event's template, in order to
  /// keep you logging structured!
  [<CompiledName "EventVerboseFormat">]
  let eventVerbosef fmt = Printf.kprintf (event Verbose) fmt

  /// Create a debug event message
  [<CompiledName "EventDebug">]
  let eventDebug = event LogLevel.Debug

  /// Create a debug event message, for help constructing format string, see:
  /// http://msdn.microsoft.com/en-us/library/vstudio/ee370560.aspx
  /// Warning: Prefer not formatting data into the event's template, in order to
  /// keep you logging structured!
  [<CompiledName "EventDebugFormat">]
  let eventDebugf fmt = Printf.kprintf (event LogLevel.Debug) fmt

  /// Create an info event message
  [<CompiledName "EventInfo">]
  let eventInfo = event Info

  /// Create a info event message, for help constructing format string, see:
  /// http://msdn.microsoft.com/en-us/library/vstudio/ee370560.aspx
  /// Warning: Prefer not formatting data into the event's template, in order to
  /// keep you logging structured!
  [<CompiledName "EventInfoFormat">]
  let eventInfof fmt = Printf.kprintf (event Info) fmt

  /// Create an warn event message
  [<CompiledName "EventWarn">]
  let eventWarn = event LogLevel.Warn

  /// Create a warn event message for help constructing format string, see:
  /// http://msdn.microsoft.com/en-us/library/vstudio/ee370560.aspx
  /// Warning: Prefer not formatting data into the event's template, in order to
  /// keep you logging structured!
  [<CompiledName "EventWarnFormat">]
  let eventWarnf fmt = Printf.kprintf (event Warn) fmt

  /// Create an error event message
  [<CompiledName "EventError">]
  let eventError = event LogLevel.Error

  /// Write a error event message, for help constructing format string, see:
  /// http://msdn.microsoft.com/en-us/library/vstudio/ee370560.aspx
  /// Warning: Prefer not formatting data into the event's template, in order to
  /// keep you logging structured!
  [<CompiledName "EventErrorFormat">]
  let eventErrorf fmt = Printf.kprintf (event LogLevel.Error) fmt

  /// Create a fatal event message
  [<CompiledName "EventFatal">]
  let eventFatal = event Fatal

  /// Create a fatal event message, for help constructing format string, see:
  /// http://msdn.microsoft.com/en-us/library/vstudio/ee370560.aspx
  /// Warning: Prefer not formatting data into the event's template, in order to
  /// keep you logging structured!
  [<CompiledName "EventFatalFormat">]
  let eventFatalf fmt = Printf.kprintf (event Fatal) fmt

  /// A destructuring strategy for FsMessageTemplates which simply treats
  /// everything as a 'Scalar' object which can later be handled by Logary
  /// as `Field (Value.create o, None)`
  let internal destructureAllAsScalar : Destructurer =
    fun request -> TemplatePropertyValue.ScalarValue request.Value

  let internal captureNamesAndValuesAsScalars (t: Template) (args: obj[]) =
    Capturing.capturePropertiesWith ignore destructureAllAsScalar 1 t args

  let internal convertToOrigin (pnv : PropertyNameAndValue) : string * obj =
    match pnv.Value with
    | ScalarValue v ->
      pnv.Name, v
    | _ ->
      failwith "In Logary we extract all properties as Scalar values. File a bug report with the parameter values that you called the function with."

  [<CompiledName "SetFields">]
  let setFields (args : obj[]) message =
    let (Event (formatTemplate)) = message.value
    let parsedTemplate = Parser.parse formatTemplate
    if parsedTemplate.HasAnyProperties then
      captureNamesAndValuesAsScalars parsedTemplate args
      |> Array.map convertToOrigin
      |> Array.fold (fun m (name, value) -> setField name value m) message
    else message



  /// Run the function `f` and measure how long it takes; logging that
  /// measurement as a Gauge in the unit Seconds.
  [<CompiledName "Time">]
  let time pointName (f : 'input -> 'res) : 'input -> 'res * Message =
    fun input ->
      let sw = Stopwatch.StartNew()
      let res = f input
      sw.Stop()

      let message = sw.toGauge() |> gaugeMessage (PointName.format pointName)
      res, message

  [<CompiledName "TimeAsync">]
  let timeAsync pointName (fn : 'input -> Async<'res>) : 'input -> Async<'res * Message> =
    fun input ->
      async {
        let sw = Stopwatch.StartNew()
        let! res = fn input
        sw.Stop()

        let message = sw.toGauge() |> gaugeMessage (PointName.format pointName)
        return res, message
      }


  [<CompiledName "TimeJob">]
  let timeJob pointName (fn : 'input -> Job<'res>) : 'input -> Job<'res * Message> =
    fun input ->
      job {
        let sw = Stopwatch.StartNew()
        let! res = fn input
        sw.Stop()

        let message = sw.toGauge() |> gaugeMessage (PointName.format pointName)
        return res, message
      }

  [<CompiledName "TimeAlt">]
  let timeAlt pointName (fn : 'input -> Alt<'res>) : 'input -> Alt<'res * Message> =
    fun input ->
    Alt.prepareFun (fun () ->
      let sw = Stopwatch.StartNew()
      fn input |> Alt.afterFun (fun res ->
      sw.Stop()

      let message = sw.toGauge() |> gaugeMessage (PointName.format pointName)
      res, message
    ))

  [<CompiledName "TimeTask">]
  let timeTask pointName (fn : 'input -> Task<'res>) : 'input -> Task<'res * Message> =
    fun input ->
      let sw = Stopwatch.StartNew()
      // http://stackoverflow.com/questions/21520869/proper-way-of-handling-exception-in-task-continuewith
      (fn input).ContinueWith((fun (task : Task<'res>) ->
        sw.Stop()
        task.Result, // will rethrow if needed
        sw.toGauge() |> gaugeMessage (PointName.format pointName)
      ), TaskContinuationOptions.ExecuteSynchronously) // stopping SW is quick

  //#endregion

  //#region PROPS

  ///////////////// PROPS ////////////////////

  /// Sets the name of the message to a PointName
  [<CompiledName "SetName">]
  let setName name (msg : Message) =
    { msg with name = name }

  /// Sets the name of the message from a string.
  [<CompiledName "SetName">]
  let setSimpleName name (msg : Message) =
    { msg with name = PointName.parse name }

  /// Sets the last bit of the Message name value to the given `nameEnding`.
  /// This is useful when you have functions calling a static logger, but you
  /// want to make the Message say what function it was created from.
  /// Note: lastBitName MAY BE NULL!
  [<CompiledName "SetNameEnding">]
  let setNameEnding (nameEnding : string) : Message -> Message = function
    | { name = pn } as m
      when not (nameEnding = null)
        && not (String.isEmpty nameEnding) ->
      { m with name = pn |> PointName.setEnding nameEnding }
    | m ->
      m

  /// Sets the message's level.
  [<CompiledName "SetLevel">]
  let setLevel lvl msg = { msg with level = lvl}

  /// Sets the number of nanoseconds since epoch.
  [<CompiledName "SetNanoEpoch">]
  let setNanoEpoch (ts : EpochNanoSeconds) msg =
    { msg with timestamp = ts }

  [<CompiledName "SetTimestamp">]
  let setTimestamp (instant : Instant) msg =
    { msg with timestamp = instant.ToUnixTimeTicks() * Constants.NanosPerTick }

  /// Sets the number of ticks since epoch. There are 10 ticks per micro-second,
  /// so a tick is a 1/10th microsecond, so it's 100 nanoseconds long.
  [<CompiledName "SetTicksEpoch">]
  let setTicksEpoch (ticks : int64) msg =
    { msg with timestamp = ticks * Constants.NanosPerTick }

  /// Sets the number of ticks as specified by DateTime and DateTimeOffset,
  /// which starts at zero the 0001-01-01 00:00:00 instant.
  [<CompiledName "SetUTCTicks">]
  let setUTCTicks (ticks : int64) msg =
    setTicksEpoch (ticks - DateTime(1970, 1, 1, 0, 0, 0, DateTimeKind.Utc).Ticks) msg

  /// Update the message with the current timestamp.
  [<CompiledName "UpdateTimestamp">]
  let updateTimestamp message =
    { message with timestamp = Global.getTimestamp () }

  /// Replaces the value of the message with a new Event with the supplied format
  [<CompiledName "SetEvent">]
  let setEvent format message =
    { message with value = Event (string format) }

  [<Obsolete ("Use addGauge instand.")>]
  [<CompiledName "SetGauge">]
  let setGauge (value, units) message =
    addGauge KnownLiterals.DefaultGaugeType (Gauge (value,units)) message

  [<Obsolete ("Use addGauge instand.")>]
  [<CompiledName "SetDerived">]
  let setDerived (value, units) message =
    setGauge (value,units) message

  /// Adds a new exception to the "errors" field in the message.
  /// AggregateExceptions are automatically expanded.
  [<CompiledName "AddException">]
  let addExn (e : exn) msg =
    let errorCtxName = KnownLiterals.ErrorsContextName
    let errors =
      match tryGetContext errorCtxName msg with
      | Some (errors:list<exn>) -> e :: errors
      | _ -> [e]

    setContext errorCtxName errors msg

  [<CompiledName "GetErrors">]
  let getErrors msg : exn list =
    match tryGetContext KnownLiterals.ErrorsContextName msg with
    | Some (errors) -> errors
    | _ -> List.empty


  //#endregion

[<AutoOpen>]
module MessageEx =

  type Message with

    /// Creates a new event with given level, format and arguments. Format may
    /// contain String.Format-esque format placeholders.
    [<CompiledName "EventFormat">]
    static member eventFormat (level, formatTemplate, [<ParamArray>] args : obj[]) : Message =
      Message.event level formatTemplate
      |> Message.setFields args

    /// Converts a String.Format-style format string and an array of arguments into
    /// a message template and a set of fields.
    [<CompiledName "EventFormat">]
    static member templateFormat (format : string, [<ParamArray>] args : obj[]) =
      Message.eventFormat (LogLevel.Debug, format, args)
    static member templateEvent<'T> (level : LogLevel, format : string) : ('T -> Message) =
      let template = Parser.parse format
      if isNull template.Named || template.Named.Length <> 1 then
        raise (System.ArgumentException (sprintf "Template '%s' must have exactly 1 named property" format))
      let field = template.Named.[0]
      fun (v : 'T) ->
        Message.event level format
        |> Message.setField field.Name v

    static member templateEvent<'T1, 'T2> (level : LogLevel, format : string) : ('T1 -> 'T2 -> Message) =
      let template = Parser.parse format
      if isNull template.Named || template.Named.Length <> 2 then
        failwithf "Template '%s' must have exactly 2 named properties" format
      let field1 = template.Named.[0]
      let field2 = template.Named.[1]
      fun (v1 : 'T1) (v2 : 'T2) ->
        Message.event level format
        |> Message.setField field1.Name v1
        |> Message.setField field2.Name v2

    static member templateEvent<'T1, 'T2, 'T3> (level : LogLevel, format : string) : ('T1 -> 'T2 -> 'T3 -> Message) =
      let template = Parser.parse format
      if isNull template.Named || template.Named.Length <> 3 then
        failwithf "Template '%s' must have exactly 3 named properties" format
      let field1 = template.Named.[0]
      let field2 = template.Named.[1]
      let field3 = template.Named.[2]
      fun (v1 : 'T1) (v2 : 'T2) (v3 : 'T3) ->
        Message.event level format
        |> Message.setField field1.Name v1
        |> Message.setField field2.Name v2
        |> Message.setField field3.Name v3

    static member templateEvent<'T1, 'T2, 'T3, 'T4> (level : LogLevel, format : string) : ('T1 -> 'T2 -> 'T3 -> 'T4 -> Message) =
      let template = Parser.parse format
      if isNull template.Named || template.Named.Length <> 4 then
        failwithf "Template '%s' must have exactly 4 named properties" format
      let field1 = template.Named.[0]
      let field2 = template.Named.[1]
      let field3 = template.Named.[2]
      let field4 = template.Named.[3]
      fun (v1 : 'T1) (v2 : 'T2) (v3 : 'T3) (v4 : 'T4) ->
        Message.event level format
        |> Message.setField field1.Name v1
        |> Message.setField field2.Name v2
        |> Message.setField field3.Name v3
        |> Message.setField field4.Name v4