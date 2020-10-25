namespace Logary.Targets

open System.Net.Http
open Logary
open Logary.Internals
open Newtonsoft.Json.Linq
open Newtonsoft.Json.Schema

// Ignore Deprecated
#nowarn "44"

open System
open System.Collections.Generic
open System.IO
open System.Net
open System.Text
open System.Threading.Tasks
open Confluent.SchemaRegistry
open Confluent.SchemaRegistry.Serdes
//open NJsonSchema
//open NJsonSchema.Validation
open Confluent.Kafka
open Hopac
open Logary.Internals.Chiron

module internal Option =
  let bindNullable (fn: 'x -> Nullable<'y>) xO =
    match xO with
    | None ->
      None
    | Some v ->
      let res = fn v
      if res.HasValue then Some res.Value else None

module internal Constants =
  let MagicByte = 0uy
  let DefaultInitialBufferSize = 1024


/// TO CONSIDER: Switch to Newtonsoft.Json.Schema
type SchemaStrategy =
  | NoSchema
  | OfURL of url: string
  | OfValue of schema: JSchema

  member x.key =
    match x with
    | NoSchema -> "NoSchema"
    | OfURL url -> sprintf "OfURL %s" url
    | OfValue s -> "OfValue"

  static member OfType<'T>(?settings) =
//    if Option.isNone settings
    //then OfValue (JsonSchema.FromType<'T>())
//    then OfValue (.FromType<'T>())
    //else OfValue (JsonSchema.FromType<'T>(Option.get settings))
    NoSchema

/// <summary>
///     JSON Serializer.
/// </summary>
/// <remarks>
///     Serialization format:
///       byte 0:           A magic byte that identifies this as a message with
///                         Confluent Platform framing.
///       bytes 1-4:        Unique global id of the JSON schema associated with
///                         the data (as registered in Confluent Schema Registry),
///                         big endian.
///       following bytes:  The JSON data (utf8)
///
///     Internally, the serializer uses Newtonsoft.Json for
///     serialization and NJsonSchema for schema creation and
///     validation. You can use any property annotations recognised
///     by these libraries.
///
///     Note: Off-the-shelf libraries do not yet exist to enable
///     integration of System.Text.Json and JSON Schema, so this
///     is not yet supported by the serializer.
/// </remarks>
type Serialiser<'T>(ilogger: Logger,
                    registry: ISchemaRegistryClient,
                    encoder: JsonEncoder<'T>,
                    config: JsonSerializerConfig,
                    schemaStrategy: SchemaStrategy) =
  let resolve = Cache.memoize <| function
    | NoSchema -> Promise None
    | OfValue value -> Promise (Some value)
    | OfURL url ->
      memo (
        job {
          ilogger.verbose "Loading schema..."
          let resolver = JSchemaUrlResolver()
          use client = new HttpClient()
          use! resp = client.GetAsync(url)
          let! schemaStr = resp.Content.ReadAsStringAsync()
          let schema = JSchema.Parse(schemaStr, resolver)
          ilogger.verbose("Loaded schema={schema}...", fun m -> m.setField("schema", schemaStr))
          return Some schema
        }
      )
  let initialBufferSize =
    config.BufferBytes |> Option.ofNullable |> Option.defaultValue Constants.DefaultInitialBufferSize
  let autoRegisterSchemas =
    config.AutoRegisterSchemas |> Option.ofNullable |> Option.defaultValue true
  let subjectNameStrategy =
    config.SubjectNameStrategy
      |> Option.ofNullable
      |> Option.map (fun strategy -> strategy.ToDelegate())
      |> Option.defaultValue null

  let subjectsRegistered = HashSet<string>()
  let registerSchemaLock = Lock()
  let EmptyReferencesList = ResizeArray<global.Confluent.SchemaRegistry.SchemaReference>()

  //let validator = JsonSchemaValidator()

  /// <remarks>
  /// A given schema is uniquely identified by a schema id, even when
  /// registered against multiple subjects.
  /// </remarks>
  let schemaId: int option ref = ref None

  let rec printErrorInner (indent: int, sb: StringBuilder) (ve: ValidationError) =
    let em = ve.GetType().GetMethod("GetExtendedMessage", Reflection.BindingFlags.NonPublic ||| Reflection.BindingFlags.Instance).Invoke(ve, [||]) :?> string
    sb.AppendLine(System.String.Format("{0}{1}", System.String(' ', indent), em)) |> ignore
    if ve.ChildErrors.Count > 0 then
      for innerVE in ve.ChildErrors do
        ignore (printErrorInner (indent + 2, sb) innerVE)
    sb

  and printErrors (ve: ValidationError) =
    let sb = printErrorInner (0, StringBuilder()) ve
    sb.ToString()


  let validate schema (jsonStr: string) =
    let jToken = JToken.Parse(jsonStr)

    // validate json
    let mutable errors = ResizeArray<ValidationError>() :> IList<_>
    if not (jToken.IsValid(schema, &errors)) then
      let errors = errors |> Seq.map printErrors |> String.concat "\n\n"
      Result.Error errors
    else
      Result.Ok ()

  let validate (jsonStr: string) (schema: JSchema) =
    //let validationResult = validator.Validate(jsonStr, schema)
    match validate schema jsonStr with
    | Result.Ok () -> ()
    | Result.Error error ->
      raise (InvalidDataException(sprintf "Schema validation failed with message: %s" error))

  let getSubjectName context (schema: JSchema) =
    if not (isNull subjectNameStrategy)
    // use the subject name strategy specified in the serializer config if available.
    then subjectNameStrategy.Invoke(context, schema.Title)
    // else fall back to the deprecated config from (or default as currently supplied by) SchemaRegistry.
    else
      if context.Component = MessageComponentType.Key
      then registry.ConstructKeySubjectName(context.Topic, schema.Title)
      else registry.ConstructValueSubjectName(context.Topic, schema.Title)

  let registerSchema (schemaStr: string, schema: JSchema) context =
    ilogger.verbose (sprintf "Validating schema: %s" schema.Title)

    do validate schemaStr schema

    let subject = getSubjectName context schema

    if subjectsRegistered.Contains subject then
      ilogger.verbose "Subject already registered"
      Job.unit ()
    else
      Lock.duringJob registerSchemaLock <| job {
        let s = Schema(schemaStr, EmptyReferencesList, SchemaType.Json)

        // first usage: register/get schema to check compatibility
        if autoRegisterSchemas then
          ilogger.debug ("Registering schema for schema={title}", fun m -> m.setField("title", schema.Title))
          try
            let! res = Job.fromTask (fun () -> registry.RegisterSchemaAsync(subject, s))
            schemaId := Some res
          with e ->
            // the schema is at least valid
            ilogger.warn ("Failed to register schema={title}.", e, fun m -> m.setField("title", schema.Title))
            e.reraise()

        else
          ilogger.debug ("Get Schema Id for schema={title}", fun m -> m.setField("title", schema.Title))
          let! res = registry.GetSchemaIdAsync(subject, s)
          schemaId := Some res

        subjectsRegistered.Add subject
          |> ignore
      }

  let maybeRegisterSchema (schemaStr: string, context): Job<_> =
    job {
      ilogger.verbose (sprintf "Resolving SchemaStrategy %s" schemaStrategy.key)
      match! resolve schemaStrategy with
      | None ->
        ilogger.verbose "NoSchema SchemaStrategy used"
      | Some schema ->
        return! registerSchema (schemaStr, schema) context
    }

  interface IAsyncSerializer<'T> with
    /// <summary>
    ///     Serialize an instance of the type 'T to a UTF8 encoded JSON
    ///     representation. The serialized data is preceeded by:
    ///       1. A "magic byte" (1 byte) that identifies this as a message with
    ///          Confluent Platform framing.
    ///       2. The id of the schema as registered in Confluent's Schema Registry
    ///          (4 bytes, network byte order).
    ///     This call may block or throw on first use for a particular topic during
    ///     schema registration / verification.
    /// </summary>
    member x.SerializeAsync(value: 'T, context: SerializationContext): Task<byte[]> =
      if isNull (box value) then Task.FromResult Array.empty else
      job {
        let json = encoder value
        let jsonStr = json |> Json.formatWith JsonFormattingOptions.Compact
        do! maybeRegisterSchema (jsonStr, context)
        use stream = new MemoryStream(initialBufferSize)
        use writer = new BinaryWriter(stream)
        match !schemaId with
        | Some sId ->
          stream.WriteByte(Constants.MagicByte)
          writer.Write(IPAddress.HostToNetworkOrder(sId))
        | None -> ()
        writer.Write(Encoding.UTF8.GetBytes(jsonStr))
        return stream.ToArray()
      }
      |> Job.ToTask