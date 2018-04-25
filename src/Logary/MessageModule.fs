namespace Logary

open Hopac
open NodaTime
open System
open System.Threading.Tasks
open System.Diagnostics
open Logary.Internals
open Logary.Internals.Aether
open Logary.Internals.Aether.Operators
open Logary.MessageTemplates
open Logary

/// Open this module to log in a more succinct way.
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Message =

  module Optic =

    let name_: Lens<Message, PointName> =
      (fun (x: Message) -> x.name),
      fun v (x: Message) -> { x with name = v }

    let value_: Lens<Message, string> =
      (fun (x: Message) -> x.value),
      fun v (x: Message) -> { x with value = v }

    let context_: Lens<Message, _> =
      (fun (x: Message) -> x.context),
      (fun v (x: Message) -> { x with context = v })

    let level_: Lens<Message, LogLevel> =
      (fun (x: Message) -> x.level),
      (fun v (x: Message) -> { x with level = v })

    let timestamp_: Lens<Message, EpochNanoSeconds> =
      (fun (x: Message) -> x.timestamp),
      (fun v (x: Message) -> { x with timestamp = v })

    let boxWithOption_<'a> : Epimorphism<obj option,'a> =
      (function | Some (:? 'a as x) -> Some x
                | Some x when isNull x -> Some (Unchecked.defaultof<obj> :?> 'a)
                | Some _ -> None
                | None -> None),
      box >> Some

    let contextValue_ name =
      context_ >-> HashMap.value_ name >-> boxWithOption_

    let contextValueObj_ name =
      context_ >-> HashMap.value_ name

  //#region CONTEXT AND FIELDS

  ///////////////// CONTEXT ////////////////////

  /// Sets a context value if name exist, value will be override
  [<CompiledName "SetContext">]
  let setContext name value message =
    if isNull name then message
    else Optic.set (Optic.contextValue_ name) value message

  let setContexts (values: #seq<string * obj>) message =
    (message, values)
    ||> Seq.fold (fun m (k, v) -> m |> setContext k v)

  [<CompiledName "SetContextFromMap">]
  let setContextFromMap (context: Map<string, obj>) message =
    context |> Seq.fold (fun m (KeyValue (k, v)) -> m |> setContext k v) message

  [<CompiledName "SetContextFromObject">]
  let setContextFromObject (o: obj) message =
    (message, Reflection.propsFrom o)
    ||> Seq.fold (fun m (label, value) -> m |> setContext label value)

  /// Tries to get a context value
  [<CompiledName "TryGetContext">]
  let tryGetContext name message =
    if isNull name then None
    else Optic.get (Optic.contextValue_ name) message

  [<CompiledName "GetContextByPrefix">]
  let getContextByPrefix (prefix: string) message =
    if String.IsNullOrEmpty prefix then
      message.context
      |> HashMap.toSeq
    else
      message.context
      |> Seq.filter (fun (KeyValue (k, _)) -> k.StartsWith prefix)
      |> Seq.map (fun (KeyValue (k, v)) -> k.Substring(prefix.Length), v)

  /// Gets all context values that are NOT:
  /// - fields
  /// - gauges
  /// - exceptions
  ///
  [<CompiledName "GetOthers">]
  let getOthers message =
    let isPrefixed (k: string) =
      k.StartsWith KnownLiterals.FieldsPrefix
      || k.StartsWith KnownLiterals.GaugeNamePrefix
      || k = KnownLiterals.ErrorsContextName

    message.context |> Seq.choose (function
      | KeyValue (k, v) when isPrefixed k ->
        None
      | KeyValue (k, v) ->
        Some (k,v))

  ///////////////// FIELDS ////////////////////

  /// Get a partial setter lens to a field
  [<CompiledName "SetField">]
  let setField name value message =
    let ctxFieldsKey = KnownLiterals.FieldsPrefix + name
    setContext ctxFieldsKey value message

  /// Sets the field with a value and its unit, on the message value.
  [<CompiledName "SetFieldUnit">]
  let setFieldUnit name value units message =
    setField name (Gauge (value,units)) message

  [<CompiledName "SetFieldsFromSeq">]
  let setFieldsFromSeq (fields: (string * obj) seq) message =
    (message, fields)
    ||> Seq.fold (fun m (name, value) -> m |> setField name value)

  [<CompiledName "SetFieldsFromMap">]
  let setFieldsFromMap (m: Map<string, obj>) message =
    (message, m)
    ||> Map.fold (fun m k v -> m |> setField k v)

  [<CompiledName "SetFieldsFromMap">]
  let setFieldsFromHashMap (m: HashMap<string, obj>) message =
    (message, m)
    ||> Seq.fold (fun m (KeyValue (k,v)) -> m |> setField k v)

  /// Reflects properties of plain old C# objects and puts those properties as
  /// fields in the Message's context.
  [<CompiledName "SetFieldsFromObject">]
  let setFieldsFromObject (o: obj) message =
    (message, Reflection.propsFrom o)
    ||> Seq.fold (fun m (label, value) -> m |> setField label value)

  [<CompiledName "TryGetField">]
  let tryGetField name message =
    let ctxFieldsKey = KnownLiterals.FieldsPrefix + name
    tryGetContext ctxFieldsKey message

  [<CompiledName "GetAllFields">]
  let getAllFields message =
    getContextByPrefix KnownLiterals.FieldsPrefix message

  // GAUGES

  [<CompiledName "GetAllGauges">]
  let getAllGauges message =
    message
    |> getContextByPrefix KnownLiterals.GaugeNamePrefix
    |> Seq.choose (function
      | k, (:? Gauge as gauge) -> Some (k, gauge)
      | _ -> None)

  [<CompiledName "GetAllTags">]
  let getAllTags message =
    tryGetContext KnownLiterals.TagsContextName message
    |> Option.orDefault (fun () -> Set.empty)

  [<CompiledName "GetAllSinks">]
  let getAllSinks message: Set<string> =
    tryGetContext KnownLiterals.SinkTargetsContextName message
    |> Option.orDefault (fun () -> Set.empty)

  [<CompiledName "GetAllSinks">]
  let addSinks (sinks: string list) message =
    let sinks = message |> getAllSinks |> Set.union (Set.ofList sinks)
    setContext KnownLiterals.SinkTargetsContextName sinks message

  /// Tag the message
  [<CompiledName "Tag">]
  let tag (tag: string) (message: Message) =
    let tags = message |> getAllTags |> Set.add tag
    setContext KnownLiterals.TagsContextName tags message

  /// Check if the Message has a tag
  [<CompiledName "HasTag">]
  let hasTag (tag: string) (message: Message) =
    message |> getAllTags |> Set.contains tag

  //#endregion

  //#region CTORS

  ///////////////// CTORS ////////////////////

  /// Create a new Message with the passed parameters. Consider using `event` and
  /// `gauge` instead.
  let create context level name value =
    let timestamp = Global.getTimestamp ()
    { context = context
      level = level
      name = name
      timestamp = timestamp
      value = value }

  /// Creates a new event Message with a specified level.
  [<CompiledName "Event">]
  let event level template =
    create HashMap.empty level PointName.empty template

  /// Creates a new event message template with level. Compared to `event`,
  /// this function has its parameters' order flipped.
  [<CompiledName "Event">]
  let eventX template level =
    event level template

  /// A single Message can take multiple gauges; use this function to add further
  /// gauges to the message. You can add gauges to events as well.
  [<CompiledName "AddGauge">]
  let addGauge gaugeName (gauge: Gauge) message =
    let gaugeName = KnownLiterals.GaugeNamePrefix + gaugeName
    message |> setContext gaugeName gauge

  /// A single Message can take multiple gauges; use this function to add further
  /// gauges to the message. You can add gauges to events as well.
  [<CompiledName "AddGauges">]
  let addGauges (gauges: #seq<string * Gauge>) message =
    (message, gauges) ||> Seq.fold (fun m (n, g) -> m |> addGauge n g)

  /// Creates a new Message with a single Gauge value and its unit (at Debug level).
  [<CompiledName "GaugeWithUnit">]
  let gaugeWithUnit sensorName gaugeName gauge =
    create HashMap.empty Debug sensorName String.Empty
    |> addGauge gaugeName gauge

  let inline gaugeWithUnitf sensorName gaugeName units fval =
    let g = Gauge (Float (float fval), units)
    gaugeWithUnit sensorName gaugeName g

  let inline gaugeWithUnitfs sensorNameStr gaugeName units fval =
    let sensorName = PointName.parse sensorNameStr
    gaugeWithUnitf sensorName gaugeName units fval

  let inline gaugeWithUniti sensorName gaugeName units ival =
    let g = Gauge (Int64 (int64 ival), units)
    gaugeWithUnit sensorName gaugeName g

  let inline gaugeWithUnitis sensorNameStr gaugeName units ival =
    let sensorName = PointName.parse sensorNameStr
    gaugeWithUniti sensorName gaugeName units ival

  /// Creates a new Message with a multiple Gauge values and their respective=
  /// units (at Debug level).
  [<CompiledName "GaugesWithUnits">]
  let gaugesWithUnits sensorName (gauges: #seq<string * Gauge>) =
    (create HashMap.empty Debug sensorName String.Empty, gauges)
    ||> Seq.fold (fun m (n, g) -> m |> addGauge n g)

  /// Creates a new Message with a single Gauge value (Debug level).
  [<CompiledName "Gauge">]
  let gauge sensorName gaugeName value =
    gaugeWithUnit sensorName gaugeName (Gauge (value, Units.Scalar))

  /// Creates a new Message with a multiple Gauge values (at Debug level).
  [<CompiledName "Gauges">]
  let gauges sensorName (gauges: #seq<string * Value>) =
    (create HashMap.empty Debug sensorName String.Empty, gauges)
    ||> Seq.fold (fun m (n, v) -> m |> addGauge n (Gauge (v, Units.Scalar)))

  /// A float-compatible gauge
  let inline gaugef sensorName gaugeName fval =
    gaugeWithUnitf sensorName gaugeName Units.Scalar fval

  let inline gaugefs sensorNameStr gaugeName fval =
    gaugeWithUnitfs sensorNameStr gaugeName Units.Scalar fval

  /// An int-compatible gauge
  let inline gaugei sensorName gaugeName ival =
    gaugeWithUniti sensorName gaugeName Units.Scalar ival

  let inline gaugeis sensorNameStr gaugeName ival =
    gaugeWithUnitis sensorNameStr gaugeName Units.Scalar ival

  [<CompiledName "TryGetGauge">]
  let tryGetGauge gaugeName message: Gauge option =
    let gaugeTypeName = KnownLiterals.GaugeNamePrefix + gaugeName
    message |> tryGetContext gaugeTypeName

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

  [<CompiledName "SetFields">]
  let setFields (args: obj[]) message =
    capture (parse message.value) args
    |> Array.fold (fun m (pt, value) ->
       match value with
       | Some v -> setField pt.name v m
       | None -> m
       ) message

  /// Run the function `f` and measure how long it takes; logging that
  /// measurement as a Gauge in the unit Seconds.
  [<CompiledName "Time">]
  let time pointName (f: 'input -> 'res) : 'input -> 'res * Message =
    fun input ->
      let sw = Stopwatch.StartNew()
      let res = f input
      sw.Stop()

      let message = sw.toGauge() |> gaugeWithUnit pointName "time"
      res, message

  [<CompiledName "TimeAsync">]
  let timeAsync pointName (fn: 'input -> Async<'res>) : 'input -> Async<'res * Message> =
    fun input ->
      async {
        let sw = Stopwatch.StartNew()
        let! res = fn input
        sw.Stop()

        let message = sw.toGauge() |> gaugeWithUnit pointName "time"
        return res, message
      }

  [<CompiledName "TimeJob">]
  let timeJob pointName (fn: 'input -> Job<'res>) : 'input -> Job<'res * Message> =
    fun input ->
      job {
        let sw = Stopwatch.StartNew()
        let! res = fn input
        sw.Stop()

        let message = sw.toGauge() |> gaugeWithUnit pointName "time"
        return res, message
      }

  [<CompiledName "TimeAlt">]
  let timeAlt pointName (fn: 'input -> Alt<'res>) : 'input -> Alt<'res * Message> =
    fun input ->
    Alt.prepareFun (fun () ->
      let sw = Stopwatch.StartNew()
      fn input |> Alt.afterFun (fun res ->
      sw.Stop()

      let message = sw.toGauge() |> gaugeWithUnit pointName "time"
      res, message
    ))

  [<CompiledName "TimeTask">]
  let timeTask pointName (fn: 'input -> Task<'res>) : 'input -> Task<'res * Message> =
    fun input ->
      let sw = Stopwatch.StartNew()
      // http://stackoverflow.com/questions/21520869/proper-way-of-handling-exception-in-task-continuewith
      (fn input).ContinueWith((fun (task: Task<'res>) ->
        sw.Stop()
        task.Result, // will rethrow if needed
        sw.toGauge() |> gaugeWithUnit pointName "time"
      ), TaskContinuationOptions.ExecuteSynchronously) // stopping SW is quick

  //#endregion

  //#region PROPS

  ///////////////// PROPS ////////////////////

  /// Sets the name of the message to a PointName
  [<CompiledName "SetName">]
  let setName name (msg: Message) =
    { msg with name = name }

  /// Sets the name of the message from a string.
  [<CompiledName "SetName">]
  let setNameStr name (msg: Message) =
    { msg with name = PointName.parse name }

  /// Sets the last bit of the Message name value to the given `nameEnding`.
  /// This is useful when you have functions calling a static logger, but you
  /// want to make the Message say what function it was created from.
  /// Note: lastBitName MAY BE NULL!
  [<CompiledName "SetNameEnding">]
  let setNameEnding (nameEnding: string): Message -> Message = function
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
  let setNanoEpoch (ts: EpochNanoSeconds) msg =
    { msg with timestamp = ts }

  [<CompiledName "SetTimestamp">]
  let setTimestamp (instant: Instant) msg =
    { msg with timestamp = instant.ToUnixTimeTicks() * Constants.NanosPerTick }

  /// Sets the number of ticks since epoch. There are 10 ticks per micro-second,
  /// so a tick is a 1/10th microsecond, so it's 100 nanoseconds long.
  [<CompiledName "SetTicksEpoch">]
  let setTicksEpoch (ticks: int64) msg =
    { msg with timestamp = ticks * Constants.NanosPerTick }

  /// Sets the number of ticks as specified by DateTime and DateTimeOffset,
  /// which starts at zero the 0001-01-01 00:00:00 instant.
  [<CompiledName "SetUTCTicks">]
  let setUTCTicks (ticks: int64) msg =
    setTicksEpoch (ticks - DateTime(1970, 1, 1, 0, 0, 0, DateTimeKind.Utc).Ticks) msg

  /// Update the message with the current timestamp.
  [<CompiledName "UpdateTimestamp">]
  let updateTimestamp message =
    { message with timestamp = Global.getTimestamp () }

  /// Replaces the value of the message with a new Event with the supplied format
  [<CompiledName "SetEvent">]
  let setEvent format message =
    { message with value = string format }

  /// Adds a new exception to the "_logary.errors" internal field in the message.
  [<CompiledName "AddException">]
  let addExn (e: exn) msg =
    let errors =
      match tryGetContext KnownLiterals.ErrorsContextName msg with
      | Some errors ->
        e :: errors
      | _ ->
        e :: []
    setContext KnownLiterals.ErrorsContextName errors msg

  /// Adds new exceptions to the "_logary.errors" internal field in the message.
  [<CompiledName "AddExceptions">]
  let addExns (es: #seq<exn>) msg =
    let errors =
      match tryGetContext KnownLiterals.ErrorsContextName msg with
      | Some errors ->
        List.ofSeq es @ errors
      | _ ->
        List.ofSeq es
    setContext KnownLiterals.ErrorsContextName errors msg

  [<CompiledName "GetExceptions">]
  let getExns msg: exn list =
    match tryGetContext KnownLiterals.ErrorsContextName msg with
    | Some (errors: exn list) ->
      errors
    | _ ->
      []

  //#endregion

  /// Patterns to match against the context; useful for extracting the data
  /// slightly more semantically than "obj"-everything. Based on the known prefixes
  /// in `KnownLiterals`.
  module Patterns =
    open KnownLiterals
    open System

    /// Pattern match the key
    let (|Intern|Field|Gauge|Tags|Context|) (KeyValue (key: string, value: obj)) =
      match key with
      | _ when key = ErrorsContextName
            || key = ServiceContextName
            || key = HostContextName
            || key = SinkTargetsContextName ->
        Intern

      | _ when key = TagsContextName ->
        let tags = unbox<Set<string>> value
        Tags tags

      | _ when key.StartsWith FieldsPrefix ->
        let k = key.Substring FieldsPrefix.Length
        Field (k, value)

      | _ when key.Equals(DefaultGaugeName, StringComparison.InvariantCulture) ->
        Gauge (String.Empty, unbox<Gauge> value)

      | _ when key.StartsWith GaugeNamePrefix ->
        let k = key.Substring GaugeNamePrefix.Length
        Gauge (k, unbox<Gauge> value)

      | _ ->
        match value with
        | :? Gauge as g ->
          Gauge (key, g)
        | _ ->
          Context (key, value)

[<AutoOpen>]
module MessageEx =

  type Message with

    /// Creates a new event with given level, format and arguments. Format may
    /// contain String.Format-esque format placeholders.
    [<CompiledName "EventFormat">]
    static member eventFormat (level, formatTemplate, [<ParamArray>] args: obj[]): Message =
      Message.event level formatTemplate
      |> Message.setFields args

    /// Converts a String.Format-style format string and an array of arguments into
    /// a message template and a set of fields.
    [<CompiledName "EventFormat">]
    static member templateFormat (format: string, [<ParamArray>] args: obj[]) =
      Message.eventFormat (LogLevel.Info, format, args)

    static member templateEvent<'T> (level: LogLevel, format: string) : ('T -> Message) =
      let template = parse format
      if  template.IsAllPositional || template.Properties.Length <> 1 then
        raise (System.ArgumentException (sprintf "Template '%s' must have exactly 1 named property" format))
      let field = template.Properties.[0]
      fun (v: 'T) ->
        Message.event level format
        |> Message.setField field.name v

    static member templateEvent<'T1, 'T2> (level: LogLevel, format: string) : ('T1 -> 'T2 -> Message) =
      let template = parse format
      if template.IsAllPositional || template.Properties.Length <> 2 then
        failwithf "Template '%s' must have exactly 2 named properties" format
      let field1 = template.Properties.[0]
      let field2 = template.Properties.[1]
      fun (v1: 'T1) (v2: 'T2) ->
        Message.event level format
        |> Message.setField field1.name v1
        |> Message.setField field2.name v2

    static member templateEvent<'T1, 'T2, 'T3> (level: LogLevel, format: string) : ('T1 -> 'T2 -> 'T3 -> Message) =
      let template = parse format
      if template.IsAllPositional || template.Properties.Length <> 3 then
        failwithf "Template '%s' must have exactly 3 named properties" format
      let field1 = template.Properties.[0]
      let field2 = template.Properties.[1]
      let field3 = template.Properties.[2]
      fun (v1: 'T1) (v2: 'T2) (v3: 'T3) ->
        Message.event level format
        |> Message.setField field1.name v1
        |> Message.setField field2.name v2
        |> Message.setField field3.name v3

    static member templateEvent<'T1, 'T2, 'T3, 'T4> (level: LogLevel, format: string) : ('T1 -> 'T2 -> 'T3 -> 'T4 -> Message) =
      let template = parse format
      if template.IsAllPositional || template.Properties.Length <> 4 then
        failwithf "Template '%s' must have exactly 4 named properties" format
      let field1 = template.Properties.[0]
      let field2 = template.Properties.[1]
      let field3 = template.Properties.[2]
      let field4 = template.Properties.[3]
      fun (v1: 'T1) (v2: 'T2) (v3: 'T3) (v4: 'T4) ->
        Message.event level format
        |> Message.setField field1.name v1
        |> Message.setField field2.name v2
        |> Message.setField field3.name v3
        |> Message.setField field4.name v4
