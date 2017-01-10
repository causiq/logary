namespace Logary

open Hopac
open NodaTime
open System
open System.Threading.Tasks
open System.Diagnostics
open Logary.Internals
open Logary.Internals.Aether
open Logary.Internals.Aether.Operators
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

    let fields_ : Lens<Message, _> =
      (fun (x : Message) -> x.fields),
      (fun v (x : Message) -> { x with fields = v })

    let context_ : Lens<Message, _> =
      (fun (x : Message) -> x.context),
      (fun v (x : Message) -> { x with context = v })

    let level_ : Lens<Message, LogLevel> =
      (fun (x : Message) -> x.level),
      (fun v (x : Message) -> { x with level = v })

    let timestamp_ : Lens<Message, EpochNanoSeconds> =
      (fun (x : Message) -> x.timestamp),
      (fun v (x : Message) -> { x with timestamp = v })

    let field_ name : Prism<Message, Field> =
      fields_ >-> HashMap.key_ name

    let fieldString_ name : Prism<Message, Field> =
      fields_ >-> HashMap.key_ (PointName.ofSingle name)

    let contextValue_ name : Prism<Message, _> =
      context_ >-> HashMap.key_ name

    /// Lens you can use to get the list of errors in this message.
    /// Also see Logary errors: https://gist.github.com/haf/1a5152b77ec64bf10fe8583a081dbbbf
    let errors_ =
      fieldString_ KnownLiterals.ErrorsFieldName
      >?> Field.Optic.value_
      >?> Value.Optic.Array_

    /// Lens to the context field 'service'.
    let service_ =
      contextValue_ KnownLiterals.ServiceContextName
      >?> Value.Optic.String_

  ///////////////// FIELDS ////////////////////

  /// Get a partial setter lens to a field
  [<CompiledName "SetField">]
  let inline setField name value message =
    Optic.set (Optic.fieldString_ name) (Field.create value) message

  /// Get a partial setter lens to a field with an unit
  [<CompiledName "SetFieldUnit">]
  let inline setFieldUnit name value units message =
    Optic.set (Optic.fieldString_ name) (Field.createUnit value units) message

  /// You can also choose to construct a Field yourself, using the object model
  /// that Logary has for its data. That way you don't have to rely on having
  /// static ToValue methods on your data objects.
  [<CompiledName "SetFieldValue">]
  let setFieldValue (name : PointName) (field : Field) message =
    Optic.set (Optic.field_ name) field message

  [<CompiledName "SetFieldValues">]
  let setFieldValues (fields : (PointName * Field) seq) message =
    fields |> Seq.fold (fun m (name, value) -> setFieldValue name value m) message

  [<CompiledName "SetFieldValuesArray">]
  let setFieldValuesArray (fields : (PointName * Field)[]) message =
    fields |> Array.fold (fun m (name, value) -> setFieldValue name value m) message

  [<CompiledName "SetFieldsFromMap">]
  let setFieldsFromMap (m : Map<string, obj>) message =
    m
    |> Seq.map (fun (KeyValue (k, v)) -> PointName.ofSingle k, Field (Value.create v, None))
    |> flip setFieldValues message

  [<CompiledName "SetFieldsFromMap">]
  let setFieldsFromHashMap (m : HashMap<string, obj>) message =
    m
    |> Seq.map (fun (KeyValue (k, v)) -> PointName.ofSingle k, Field (Value.create v, None))
    |> flip setFieldValues message

  [<CompiledName "SetFieldFromObject">]
  let setFieldFromObject name (data : obj) message =
    setFieldValue name (Field (Value.create data, None)) message

  /// Reflects over the object and sets the appropriate fields.
  [<CompiledName "SetFieldsFromObject">]
  let setFieldsFromObject (data : obj) message =
    Map.ofObject data
    |> Seq.map (fun (KeyValue (k, v)) -> PointName.ofSingle k, Field (Value.create v, None))
    |> flip setFieldValues message

  /// Get a partial getter lens to a field
  [<CompiledName "TryGetField">]
  let tryGetField name message =
    Optic.get (Optic.fieldString_ name) message

  /// Tag the message
  [<CompiledName "Tag">]
  let tag (tag : string) (message : Message) =
    let key = KnownLiterals.TagsContextName
    match message.context |> HashMap.tryFind key with
    | Some (Array tags) when List.contains (String tag) tags ->
      message
    | Some (Array tags) ->
      let tags' = Array (String tag :: tags)
      { message with context = message.context |> HashMap.set key tags' }
    | Some _ ->
      message
    | None ->
      { message with context = message.context |> HashMap.set key (Array [ String tag ] ) }

  /// Check if the Message has a tag
  [<CompiledName "HasTag">]
  let hasTag (tag : string) (message : Message) =
    match message.context |> HashMap.tryFind KnownLiterals.TagsContextName with
    | Some (Array tags) when List.contains (String tag) tags -> true
    | _ -> false

  ///////////////// CONTEXT ////////////////////

  /// Sets a context value by trying to find the ToValue method on the type
  /// passed.
  [<CompiledName "SetContext">]
  let setContext name value message =
    Optic.set (Optic.contextValue_ name) (Value.create value) message

  /// Sets a context value.
  [<CompiledName "SetContextValue">]
  let setContextValue name value message =
    Optic.set (Optic.contextValue_ name) value message

  [<CompiledName "SetContextValues">]
  let setContextValues (values : (string * Value) seq) message =
    values |> Seq.fold (fun m (name, value) -> setContextValue name value m) message

  [<CompiledName "SetContextFromMap">]
  let setContextFromMap (m : Map<string, obj>) message =
    m
    |> Seq.map (fun (KeyValue (k, v)) -> k, Value.create v)
    |> List.ofSeq
    |> fun fields -> setContextValues fields message

  /// Uses reflection to set all
  [<CompiledName "SetContextFromObject">]
  let setContextFromObject (data : obj) message =
    Map.ofObject data
    |> Seq.map (fun (KeyValue (k, v)) -> k, Value.create v)
    |> List.ofSeq
    |> fun values -> setContextValues values message

  /// Tries to get a context value
  [<CompiledName "TryGetContext">]
  let inline tryGetContext name message =
    Optic.get (Optic.contextValue_ name) message

  ///////////////// CTORS ////////////////////

  /// Creates a new event message template with level
  [<CompiledName "Event">]
  let event level template =
    { name      = PointName.empty
      value     = Event template
      fields    = HashMap.empty
      context   = HashMap.empty
      level     = level
      timestamp = Global.getTimestamp () }

  /// Creates a new event message template with level. Compared to `event`,
  /// this function has its parameters' order flipped.
  [<CompiledName "Event">]
  let eventX template level =
    event level template

  /// Creates a new gauge message with data point name, unit and value
  [<CompiledName "Gauge">]
  let gaugeWithUnit dp value units =
    { name      = dp
      value     = Gauge (value, units)
      fields    = HashMap.empty
      context   = HashMap.empty
      level     = LogLevel.Debug
      timestamp = Global.getTimestamp () }

  /// Creates a new gauge message with data point name and scalar value
  [<CompiledName "Gauge">]
  let gauge dp value =
    { name      = dp
      value     = Gauge (value, Units.Scalar)
      fields    = HashMap.empty
      context   = HashMap.empty
      level     = LogLevel.Debug
      timestamp = Global.getTimestamp () }

  [<CompiledName "Derived">]
  let derivedWithUnit dp value units =
    { name      = dp
      value     = Derived (value, units)
      fields    = HashMap.empty
      context   = HashMap.empty
      level     = LogLevel.Debug
      timestamp = Global.getTimestamp () }

  [<CompiledName "Derived">]
  let derived dp value =
    { name      = dp
      value     = Derived (value, Units.Scalar)
      fields    = HashMap.empty
      context   = HashMap.empty
      level     = LogLevel.Debug
      timestamp = Global.getTimestamp () }

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

  open Logary.Internals.FsMessageTemplates

  /// A destructuring strategy for FsMessageTemplates which simply treats
  /// everything as a 'Scalar' object which can later be handled by Logary
  /// as `Field (Value.create o, None)`
  let internal destructureAllAsScalar : Destructurer =
    fun request -> TemplatePropertyValue.ScalarValue request.Value

  let internal captureNamesAndValuesAsScalars (t: Template) (args: obj[]) =
    Capturing.capturePropertiesWith ignore destructureAllAsScalar 1 t args

  let internal convertToNameAndField (pnv : PropertyNameAndValue) : PointName * Field =
    match pnv.Value with
    | ScalarValue v ->
      PointName.ofSingle pnv.Name, Field (Value.create v, None)
    | _ ->
      failwith "In Logary we extract all properties as Scalar values. File a bug report with the parameter values that you called the function with."

  let internal extractFields formatTemplate args =
    let parsedTemplate = Parser.parse formatTemplate
    captureNamesAndValuesAsScalars parsedTemplate args
    |> Array.map convertToNameAndField

  /// Creates a new event with given level, format and arguments. Format may
  /// contain String.Format-esque format placeholders.
  [<CompiledName "EventFormat">]
  let eventFormat (level, formatTemplate, [<ParamArray>] args : obj[]) : Message =
    let fields = extractFields formatTemplate args
    event level formatTemplate |> setFieldValuesArray fields

  /// Converts a String.Format-style format string and an array of arguments into
  /// a message template and a set of fields.
  [<CompiledName "EventFormat">]
  let templateFormat (format : string, [<ParamArray>] args : obj[]) =
    eventFormat (LogLevel.Debug, format, args)

  /// Run the function `f` and measure how long it takes; logging that
  /// measurement as a Gauge in the unit Seconds.
  [<CompiledName "Time">]
  let time pointName (f : 'input -> 'res) : 'input -> 'res * Message =
    fun input ->
      let sw = Stopwatch.StartNew()
      let res = f input
      sw.Stop()

      let message = sw.toGauge() ||> gaugeWithUnit pointName
      res, message

  [<CompiledName "TimeAsync">]
  let timeAsync pointName (fn : 'input -> Async<'res>) : 'input -> Async<'res * Message> =
    fun input ->
      async {
        let sw = Stopwatch.StartNew()
        let! res = fn input
        sw.Stop()

        let message = sw.toGauge() ||> gaugeWithUnit pointName
        return res, message
      }


  [<CompiledName "TimeJob">]
  let timeJob pointName (fn : 'input -> Job<'res>) : 'input -> Job<'res * Message> =
    fun input ->
      job {
        let sw = Stopwatch.StartNew()
        let! res = fn input
        sw.Stop()

        let message = sw.toGauge() ||> gaugeWithUnit pointName
        return res, message
      }

  [<CompiledName "TimeAlt">]
  let timeAlt pointName (fn : 'input -> Alt<'res>) : 'input -> Alt<'res * Message> =
    fun input ->
    Alt.prepareFun (fun () ->
      let sw = Stopwatch.StartNew()
      fn input |> Alt.afterFun (fun res ->
      sw.Stop()

      let message = sw.toGauge() ||> gaugeWithUnit pointName
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
        sw.toGauge() ||> gaugeWithUnit pointName
      ), TaskContinuationOptions.ExecuteSynchronously) // stopping SW is quick

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
    { msg with timestamp = instant.Ticks * Constants.NanosPerTick }

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
    { message with value = Event format }

  [<CompiledName "SetGauge">]
  let setGauge (value, units) message =
    { message with value = Gauge (value, units) }

  [<CompiledName "SetDerived">]
  let setDerived (value, units) message =
    { message with value = Derived (value, units) }

  /// Adds a new exception to the "errors" field in the message.
  /// AggregateExceptions are automatically expanded.
  [<CompiledName "AddException">]
  let addExn (e : exn) msg =
    let flattenedExns =
      match e with
      | :? AggregateException as ae ->
        ae.InnerExceptions |> Seq.map Value.create |> Seq.toList
      | _ ->
        Value.create e :: []

    let exnsNext =
      Optic.get Optic.errors_ msg
      |> Option.fold (fun fnes exns -> exns @ fnes) flattenedExns

    // If there's no "errors" field, add it
    let msg =
      match Optic.get Optic.errors_ msg with
      | Some x ->
        msg
      | None ->
        let name = KnownLiterals.ErrorsFieldName
        setFieldValue (PointName.ofSingle name) (Field (Array [], None)) msg

    Optic.set Optic.errors_ exnsNext msg

open Logary.Internals.FsMessageTemplates

module internal LogaryCapturing =

  let rec convertTemplatePropertyToField (tpv : TemplatePropertyValue) : Value =
    match tpv with
    | ScalarValue v ->
      // TODO: consider types like Guid, DateTime, DateTimeOffset. Are they prematurely stringified in Value.ofObject?
      // Does that prevent us from using the message template format string later in the pipeline?
      Value.create v

    | DictionaryValue kvpList ->
      let stringObjMap =
        kvpList
        |> List.map (fun (k, v) ->
          let keyValue = match k with | ScalarValue v -> v.ToString() | _ -> failwith "only scalar value keys are supported"
          keyValue, (convertTemplatePropertyToField v))
        |> Map.ofList
      Value.Object stringObjMap

    | SequenceValue tpvList ->
      Value.Array (tpvList |> List.map (convertTemplatePropertyToField))

    | StructureValue (typeTag, pnvList) ->
      Value.Object (
        pnvList
        |> List.map (fun pnv -> pnv.Name, convertTemplatePropertyToField pnv.Value)
        |> List.append [ "_typeTag", Value.String typeTag ]
        |> Map.ofList)

  let rec convertToNameAndField (pnv : PropertyNameAndValue) : string * Field =
    pnv.Name, Field ((convertTemplatePropertyToField pnv.Value), None)

  let capture (template : Internals.FsMessageTemplates.Template) ([<ParamArray>] args : obj[]) =
    Capturing.captureProperties template args
    |> Seq.map (convertToNameAndField)
    |> List.ofSeq

  let logaryDefaultDestructurer : Destructurer =
    Capturing.createCustomDestructurer (None) (Some Capturing.destructureFSharpTypes)

  let captureField (property : Internals.FsMessageTemplates.Property) value =
    logaryDefaultDestructurer (DestructureRequest (logaryDefaultDestructurer, value, 10, 0, property.Destr))
    |> convertTemplatePropertyToField
    |> fun f -> Field (f, None)

[<AutoOpen>]
module MessageEx =

  type Message with

    static member templateEvent<'T> (level : LogLevel, format : string) : ('T -> Message) =
      let template = Parser.parse format
      if isNull template.Named || template.Named.Length <> 1 then
        raise (System.ArgumentException (sprintf "Template '%s' must have exactly 1 named property" format))
      let field = template.Named.[0]
      fun (v : 'T) ->
        Message.event level format
        |> Message.setFieldValue (PointName.ofSingle field.Name) (LogaryCapturing.captureField field v)

    static member templateEvent<'T1, 'T2> (level : LogLevel, format : string) : ('T1 -> 'T2 -> Message) =
      let template = Parser.parse format
      if isNull template.Named || template.Named.Length <> 2 then
        failwithf "Template '%s' must have exactly 2 named properties" format
      let field1 = template.Named.[0]
      let field2 = template.Named.[1]
      fun (v1 : 'T1) (v2 : 'T2) ->
        let p = PointName.ofSingle
        Message.event level format
        |> Message.setFieldValue (p field1.Name) (LogaryCapturing.captureField field1 v1)
        |> Message.setFieldValue (p field2.Name) (LogaryCapturing.captureField field2 v2)

    static member templateEvent<'T1, 'T2, 'T3> (level : LogLevel, format : string) : ('T1 -> 'T2 -> 'T3 -> Message) =
      let template = Parser.parse format
      if isNull template.Named || template.Named.Length <> 3 then
        failwithf "Template '%s' must have exactly 3 named properties" format
      let field1 = template.Named.[0]
      let field2 = template.Named.[1]
      let field3 = template.Named.[2]
      fun (v1 : 'T1) (v2 : 'T2) (v3 : 'T3) ->
        let p = PointName.ofSingle
        Message.event level format
        |> Message.setFieldValue (p field1.Name) (LogaryCapturing.captureField field1 v1)
        |> Message.setFieldValue (p field2.Name) (LogaryCapturing.captureField field2 v2)
        |> Message.setFieldValue (p field3.Name) (LogaryCapturing.captureField field3 v3)

    static member templateEvent<'T1, 'T2, 'T3, 'T4> (level : LogLevel, format : string) : ('T1 -> 'T2 -> 'T3 -> 'T4 -> Message) =
      let template = Parser.parse format
      if isNull template.Named || template.Named.Length <> 4 then
        failwithf "Template '%s' must have exactly 4 named properties" format
      let field1 = template.Named.[0]
      let field2 = template.Named.[1]
      let field3 = template.Named.[2]
      let field4 = template.Named.[3]
      fun (v1 : 'T1) (v2 : 'T2) (v3 : 'T3) (v4 : 'T4) ->
        let p = PointName.ofSingle
        Message.event level format
        |> Message.setFieldValue (p field1.Name) (LogaryCapturing.captureField field1 v1)
        |> Message.setFieldValue (p field2.Name) (LogaryCapturing.captureField field2 v2)
        |> Message.setFieldValue (p field3.Name) (LogaryCapturing.captureField field3 v3)
        |> Message.setFieldValue (p field4.Name) (LogaryCapturing.captureField field4 v4)