namespace Logary.Codecs

open Logary
open Logary.Message
open Logary.Ingestion
open Logary.Internals
open Logary.Internals.Chiron
open Logary.Formatting

type Codec = Ingested -> Result<Message, string>

type Codec<'err> = Ingested -> Result<Message, 'err>

module Codec =
  /// Converts a typed codec to one returning JSON string as the errors.
  let toJsonStringError (c: Codec<'err>): Codec =
    fun input ->
      c input
      |> Result.mapError (Json.encode >> Json.format)

  /// A codec that reads each input as a bag-of-fields to add to a message. Uses
  /// the function `Json.decodeMessage` from `Logary.Formatting` in the background.
  let json: Codec<JsonFailure> =
    fun input ->
      let line = input.utf8String ()
      match Json.parse line |> JsonResult.bind Json.decodeMessage with
      | JPass message ->
        Ok message
      | JFail failure ->
        Result.Error failure

  /// A codec that reads each input as a single message with the message's value
  /// equal to the input.
  let plain: Codec =
    fun input ->
      let value = input.utf8String()
      Ok (event Debug value)

  type Log4JMessage =
    { logger: string
      /// Epoch in milliseconds * e6 to nanos in Logary
      timestamp: int64
      level: LogLevel
      message: string
      properties: HashMap<string, obj> }

    member x.normalise(): Message =
      { context = x.properties
        level = x.level
        name = PointName.parse x.logger
        timestamp = x.timestamp * 1_000_000L
        value = x.message }

  module internal Log4JMessage =
    open System
    open System.Xml
    open System.Xml.Linq

    let xn (ns: XNamespace) tag =
      XName.Get(tag, ns.NamespaceName)

    let xattr name (el: XElement) =
      if isNull el || String.IsNullOrWhiteSpace name then "" else
      let a = el.Attribute(XName.Get name)
      if isNull a then "" else a.Value

    let xe (ns: XNamespace) name (doc: XElement) =
      if isNull doc || String.IsNullOrWhiteSpace name then null else
      doc.Element (xn ns name)

    let xes (name: string) (el: XElement): seq<XElement> =
      if isNull el || String.IsNullOrWhiteSpace name then Seq.empty else
      el.Nodes()
      |> Seq.choose (function
        | :? XElement as xel ->
          Some xel
        | _ ->
          None)
      |> Seq.filter (fun xel ->
        String.Equals(xel.Name.LocalName, name, StringComparison.InvariantCultureIgnoreCase))

    let xtext (e: XElement) =
      if isNull e then "" else
      match e.FirstNode with
      | :? XText as txt ->
        txt.Value
      | other ->
        ""

    let tryTimestamp (s: string) =
      if String.IsNullOrWhiteSpace s then
        Global.getTimestamp() / 1_000_000L
      else
        match Int64.TryParse s with
        | false, _ ->
          Global.getTimestamp() / 1_000_000L
        | true, value ->
          value

    let ns = XNamespace.Get "http://jakarta.apache.org/log4j/"

    let xelement (xml: string) =
      // https://logging.apache.org/log4php/docs/layouts/xml.html
      let mngr = new XmlNamespaceManager(new NameTable())
      mngr.AddNamespace( "log4j", ns.NamespaceName)
      let parseCtx = new XmlParserContext(null, mngr, null, XmlSpace.None)
      let reader = new XmlTextReader(xml, XmlNodeType.Element, parseCtx)
      XElement.Load reader

    let inline addLiteral k v (m: HashMap<_, _>) =
      (box v, m) ||> HashMap.add (sprintf "%s%s" KnownLiterals.FieldsPrefix k)

    let foldProp acc e =
      let key = e |> xattr "name"
      (e |> xattr "value", acc) ||> addLiteral key

    let parseInner xml =
      if String.IsNullOrWhiteSpace xml then Result.Error "Log4j event was empty" else
      let event = xelement xml
      if isNull event || event.Name <> xn ns "event" then
        Result.Error (sprintf "Failed to parse XML: %s" xml)
      else
        { logger = event |> xattr "logger"
          timestamp = event |> xattr "timestamp" |> tryTimestamp
          level = event |> xattr "level" |> LogLevel.ofString
          message = event |> xe ns "message" |> xtext
          properties =
            let thread = event |> xattr "thread"
            let ndc = event |> xe ns "NDC" |> xtext
            let throwable = event |> xe ns "throwable" |> xtext |> DotNetStacktrace.parse
            (HashMap.empty, event |> xe ns "properties" |> xes "data")
            ||> Seq.fold foldProp
            |> if thread = "" then id else addLiteral "thread" thread
            |> if ndc = "" then id else addLiteral "NDC" ndc
            |> if Array.isEmpty throwable then id else addLiteral "error" throwable
        }
        |> Result.Ok

    let parse (xml: string): Result<Log4JMessage, string> =
      try parseInner xml
      with
      | :? XmlException as xmle -> Result.Error (xmle.ToString())
      | :? InvalidOperationException as oee -> Result.Error (oee.ToString())

  let log4jXML: Codec =
    fun input ->
      input.utf8String ()
      |> Log4JMessage.parse
      |> Result.map (fun log4jm -> log4jm.normalise ())

  // let regex: Codec
  // let csv: Codec