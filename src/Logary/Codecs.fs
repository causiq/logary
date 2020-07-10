namespace Logary.Codecs

open System.Collections.Generic
open Logary
open Logary.Ingestion
open Logary.Internals
open Logary.Internals.Chiron
open Logary.Model

type Codec = Ingested -> Result<LogaryMessageBase[], string>

type Codec<'err> = Ingested -> Result<LogaryMessageBase[], 'err>

module Codec =
  /// A codec that reads each input as a bag-of-fields to add to a message. Uses
  /// the function `Json.decodeMessage` from `Logary.Formatting` in the background.
  ///
  /// REMEMBER: you should use `Message.setReceiveTimestamp` to set the receive timestamp, after
  /// decoding!
  let json: Codec =
    fun input ->
      let line = input.utf8String ()
      match Json.parse line |> JsonResult.bind Json.Decode.messageBatch with
      | JPass messages ->
        Ok messages
      | JFail failure ->
        Result.Error (JsonFailure.summarize failure)

  /// A codec that reads each input as a single message with the message's value
  /// equal to the input.
  ///
  /// REMEMBER: you should use `Message.setReceiveTimestamp` to set the receive timestamp, after
  /// decoding!
  let plain: Codec =
    fun input ->
      input.utf8String()
        |> Model.Event
        :> LogaryMessageBase
        |> Array.singleton
        |> Ok

  type Log4JMessage =
    { logger: string
      /// Epoch in milliseconds * e6 to nanos in Logary
      timestamp: int64
      level: LogLevel
      message: string
      error: ErrorInfo option
      properties: IReadOnlyDictionary<string, Value> }

    member x.normalise(): Model.Event =
      Model.Event(x.message, None, x.timestamp * 1_000_000L,
                  ?name = Some (PointName.parse x.logger),
                  ?level = Some x.level,
                  ?fs = Some x.properties,
                  ?error=x.error)

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
      | _ ->
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
      let mngr = XmlNamespaceManager(NameTable())
      mngr.AddNamespace( "log4j", ns.NamespaceName)
      let parseCtx = XmlParserContext(null, mngr, null, XmlSpace.None)
      let reader = new XmlTextReader(xml, XmlNodeType.Element, parseCtx)
      XElement.Load reader

    let inline addLiteral k v (m: Map<_, _>) =
      (Value.Str v, m) ||> Map.add k

    let foldProp acc e =
      let key = e |> xattr "name"
      (e |> xattr "value", acc) ||> addLiteral key

    let parseInner xml =
      if String.IsNullOrWhiteSpace xml then Result.Error "Log4j event was empty" else
      let event = xelement xml
      if isNull event || event.Name <> xn ns "event" then
        Result.Error (sprintf "Failed to parse XML: %s" xml)
      else
        let error =
          event
            |> xe ns "throwable"
            |> xtext
            |> fun s -> if String.IsNullOrWhiteSpace(s) then None else Some s
            |> Option.bind (DotNetStacktrace.tryParse)

        { logger = event |> xattr "logger"
          timestamp = event |> xattr "timestamp" |> tryTimestamp
          level = event |> xattr "level" |> LogLevel.ofString
          message = event |> xe ns "message" |> xtext
          error = error
          properties =
            let thread = event |> xattr "thread"
            let ndc = event |> xe ns "NDC" |> xtext
            (Map.empty, event |> xe ns "properties" |> xes "data")
            ||> Seq.fold foldProp
            |> if thread = "" then id else addLiteral "thread" thread
            |> if ndc = "" then id else addLiteral "NDC" ndc
            :> IReadOnlyDictionary<string, Value>
        }
        |> Result.Ok

    let parse (xml: string): Result<Log4JMessage, string> =
      try parseInner xml
      with
      | :? XmlException as xmlE -> Result.Error (xmlE.ToString())
      | :? InvalidOperationException as oee -> Result.Error (oee.ToString())

  /// A codec that parses a string as XML and then uses the log4xml schema to extract information.
  ///
  /// REMEMBER: you should use `Message.setReceiveTimestamp` to set the receive timestamp, after
  /// decoding!
  let log4jXML: Codec =
    fun input ->
      input.utf8String ()
      |> Log4JMessage.parse
      |> Result.map (fun log4jm -> log4jm.normalise () :> LogaryMessageBase)
      |> Result.map Array.singleton

  // let regex: Codec
  // let csv: Codec