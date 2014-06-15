namespace Logary.Target

open Types

// https://github.com/racker/restkin
// https://github.com/racker/tryfer
/// A target for RestKin.
module RestKin =
  open Logary
  open Logary.Targets
  open Logary.Formatting
  open Logary.Internals.InternalLogger

  open System

  open FSharp.Actor
  open RestSharp
  open Newtonsoft.Json

  type internal hex16long =
    { value : uint64 }

  type internal Hex16LongConverter() =
    inherit JsonConverter()

    override x.CanConvert typ =
      typ = typeof<hex16long>

    override x.WriteJson(writer, value, serializer) =
      let v = value :?> hex16long
      writer.WriteValue( v.value.ToString("X") )

    override x.ReadJson(reader, t, _, serialiser) =
      match reader.TokenType with
      | JsonToken.String ->
        System.Convert.ToInt64(reader.ReadAsString(), 16) |> box
      | _ as t -> failwithf "invalid token %A when trying to read uint64/ulong" t

  open Microsoft.FSharp.Reflection

  /// Returns the case name of the object with union type 'ty.
  let private unionCaseName (x:'a) = 
    match FSharpValue.GetUnionFields(x, typeof<'a>) with
    | case, _ -> case.Name

  let private unionFromString<'a> (s:string) =
    match FSharpType.GetUnionCases typeof<'a> |> Array.filter (fun case -> case.Name.ToLowerInvariant() = s.ToLowerInvariant()) with
    |[|case|] -> Some(FSharpValue.MakeUnion(case,[||]))
    |_ -> None

  type internal DiscUnionLowerCaseString<'a>() =
    inherit JsonConverter()

    override x.CanConvert t = typeof<'a> = t

    override x.WriteJson(writer, value, serialiser) =
      let name = unionCaseName(value :?> 'a).ToLowerInvariant()
      writer.WriteValue(name)

    override x.ReadJson(reader, t, _, serialiser) =
      match reader.TokenType with
      | JsonToken.String -> unionFromString<'a> (reader.ReadAsString()) |> box
      | _ as t -> failwithf "invalid token %A when trying to read union case for %s" t (typeof<'a>.Name)

  type internal RestKinSpan =
    { trace_id    : hex16long
      span_id     : hex16long
      parent_id   : hex16long option
      name        : string
      annotations : RestKinAnnotation list }

  and internal RestKinAnnotation =
    { key : string
      ``type`` : RestKinAnnotationType }

  and internal RestKinAnnotationType =
    | String
    | Bytes
    | Timestamp

  let private makeFormatter () =
    let settings =
      JsonFormatter.Settings(
        fun opts ->
          opts.Converters.Add <| Hex16LongConverter()
          opts.Converters.Add <| DiscUnionLowerCaseString<RestKinAnnotationType>()
          opts)
    fun (o : obj) -> JsonConvert.SerializeObject(o, settings)

  /// Try find a value by key in a map
  let private lookUp (target : Map<'b,'a>) key =
    match target.TryFind key with
    | Some v -> unbox v
    | None -> None

  /// Try find a value by key in a map
  let private (?) (target : Map<'b,'a>) key =
    lookUp target key

  module internal Option =
    let orDefault a =
      Option.fold (fun s t -> t) a

  open Option

  // https://github.com/restsharp/RestSharp/wiki/Deserialization
  let private makeReq formatter tenant span =
    let r = RestRequest("/v1.0/{tenant_id}/trace", Method.POST)
             .AddHeader("Content-Type", "application/json")
             .AddUrlSegment("tenant_id", tenant)
    r.RequestFormat <- DataFormat.Json
    r.JsonSerializer <-
      { new Serializers.ISerializer with
        member x.Serialize o = formatter o
        member x.RootElement with get() = "" and set(v) = ()
        member x.ContentType with get () = "application/json" and set(v) = ()
        member x.Namespace with get () = "logary" and set(v) = ()
        member x.DateFormat with get () = "" and set(v) = () }
    r.AddBody span

  /// Configuration for RestKin target.
  type RestKinConf =
    { baseUri   : Uri
      clientFac : Uri -> IRestClient
      formatter : obj -> string }
    /// Create a new configuration with a base uri and an optional
    /// rest client factory.
    /// Default port: 6956
    /// Default host: localhost
    static member Create(?baseUri, ?fac, ?traceReq, ?formatter) =
      { baseUri   = defaultArg baseUri (Uri("http://localhost:6956"))
        clientFac = defaultArg fac (fun uri -> new RestClient(uri.ToString()) :> IRestClient)
        formatter = defaultArg formatter (makeFormatter()) }
    static member Default = RestKinConf.Create()

  type private RestKinState =
    { client : IRestClient }

  let private makeSpan (l : LogLine) =
    // TODO: map name (path)
    // TODO: map trace id
    // TODO: map span
    // TODO: map annotations
    // TODO: map binary annotations
    // TODO: map parent id
    // TODO: map debug flag
    ()

  let private restKinLoop
    { baseUri   = baseUri
      clientFac = clientFac
      formatter = formatter }
    metadata =
    (fun (inbox : IActor<_>) ->
      let rec loop state = async {
        let { client = client } = state
        let! msg, opts = inbox.Receive()
        match msg with
        | Log l ->
          let tenant = l.data?tenant |> orDefault "1"
          let traceReq = makeReq formatter tenant (makeSpan l)
          let! resp = client.PostTaskAsync traceReq
          return! loop state
        | Metric m ->
          return! loop state
        | Flush chan ->
          return! loop state
        | ShutdownTarget ackChan -> return () }

      loop { client = clientFac baseUri })

  /// Create a new RestKin target
  let [<CompiledName "Create">] create conf =
    TargetUtils.stdNamedTarget (restKinLoop conf)

  /// Use with LogaryFactory.New( s => s.Target< HERE >() )
  type Builder(conf, callParent : FactoryApi.ParentCallback<Builder>) =
    member x.WithConfig(conf : RestKinConf) =
      ! (callParent <| Builder(conf, callParent))

    new(callParent : FactoryApi.ParentCallback<_>) =
      Builder(RestKinConf.Default, callParent)

    interface Logary.Targets.FactoryApi.SpecificTargetConf with
      member x.Build name = create conf name
