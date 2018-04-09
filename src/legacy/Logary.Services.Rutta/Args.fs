namespace Logary.Services.Rutta

#nowarn "49"
// ignore "Uppercase variable identifiers should not generally be used in patterns, and may indicate a misspelt pattern name."

open Argu
open System.Reflection
[<assembly: AssemblyTitle("Logary Rutta – a router/proxy/shipper for Windows and Unix")>]
()

type RMode =
  /// zmq PULL socket. Does a PULL socket bind call.
  | Pull
  /// zmq SUB socket. Does a SUB connect call.
  | Sub
  /// TCP listener, aka zmq STREAM socket.
  | TCP
  /// UDP listener
  | UDP
  /// HTTP listener
  | HTTP

type Codec =
  /// Uses the newline-separated JSON codec
  | Json
  /// Takes the newline-separates message and create a simple message from it
  | Plain
  /// Log4J-XML input; each event should be newline-separated.
  | Log4jXML
  /// Used for Rutta-to-Rutta communication with the ZMQ socket types
  | Binary

type ProxyArgs =
  | [<Mandatory; Unique>] Xsub_Connect_To of endpoint:string
  | [<Mandatory; Unique>] Xpub_Bind of binding:string

  interface IArgParserTemplate with
    member x.Usage =
      match x with
      | Xsub_Connect_To _ ->
        "Connect in XSUB mode for PUB sockets to publish to."
      | Xpub_Bind _ ->
        "Bind in XPUB mode for SUB sockets to connect to."

module Help =
  open System
  open System.Text
  open FSharp.Reflection

  let describeMode = function
    | Pull _ -> "Runs Rutta in Router mode (PULL fan-in of Messages, forward to Target)."
    | Sub _ -> "Runs Rutta in Router XSUB mode for PUB sockets to publish to."
    | TCP _ -> "Runs Rutta in TCP STREAM server mode, that allows zmq to be used as a TCP socket. Send newline-separated JSON to this bound socket. See http://api.zeromq.org/4-0:zmq-socket#toc19"
    | UDP _ -> "Runs Rutta in UDP server mode. Send newline-separated JSON datagrams to this socket."
    | HTTP _ -> "Runs Rutta in HTTP server mode. Send JSON to /i/logary to have it converted to Messages."

  let describeModes () =
    let folder (sb: StringBuilder) (u: RMode) =
      let mInfo, _ = FSharpValue.GetUnionFields(u, typeof<RMode>)
      let mdesc = describeMode u
      sb.AppendFormat(" - {0}: {1}{2}", mInfo.Name.ToLowerInvariant(), mdesc, Environment.NewLine)
    [ Pull; Sub; TCP; UDP; HTTP ]
    |> Seq.fold folder (StringBuilder())
    |> sprintf "%O"

  let describeCodec = function
    | Json -> "Uses the newline-separated JSON codec"
    | Plain -> "Takes the newline-separates message and create a simple message from it"
    | Binary -> "Used for Rutta-to-Rutta communication with the ZMQ socket types"
    | Log4jXML -> "Takes Log4j XML event, separated with newlines, as input"

  let describeCodecs () =
    let folder (sb: StringBuilder) (u: Codec) =
      let mInfo, _ = FSharpValue.GetUnionFields(u, typeof<Codec>)
      let mdesc = describeCodec u
      sb.AppendFormat(" - {0}: {1}{2}", mInfo.Name.ToLowerInvariant(), mdesc, Environment.NewLine)
    [ Json; Plain; Binary; Log4jXML ]
    |> Seq.fold folder (StringBuilder())
    |> sprintf "%O"

[<RequireQualifiedAccess>]
type RouterArgs =
  | [<Mandatory>] Listener of routerMode:RMode * bindingOrEndpoint:string * codec:Codec
  | [<Mandatory>] Target of targetUri:string

  interface IArgParserTemplate with
    member x.Usage =
      match x with
      | Listener _ ->
        sprintf "Specifies a single listener which is a triple of a router mode (for the binding), the binding (e.g. \"127.0.0.1:20001\"), and a codec. Modes:\n%sCodecs:\n%s"
                (Help.describeModes ())
                (Help.describeCodecs ())
      | Target _ ->
        "Specifies a list of targets to ship to."

type ShipperArgs =
  | [<Unique>] Push_To of pushConnectToSocket:string
  | [<Unique>] Pub_To of pubBindSocket:string

  interface IArgParserTemplate with
    member x.Usage =
      match x with
      | Push_To _ -> "Runs Rutta in Shipper/PUSH mode (send Messages from a node to router)"
      | Pub_To _ -> "Runs Rutta in Shipper/PUB mode (send Messages from a node to proxy)"

type Args =
  | [<AltCommandLine "-V"; Inherit; Unique>] Version
  | [<AltCommandLine "-v"; Inherit; Unique>] Verbose
  | [<CliPrefix(CliPrefix.None)>] Proxy of args:ParseResults<ProxyArgs>
  | [<CliPrefix(CliPrefix.None)>] Router of args:ParseResults<RouterArgs>
  | [<CliPrefix(CliPrefix.None)>] Shipper of args:ParseResults<ShipperArgs>
  | [<Unique>] Health of binding:string
with
  interface IArgParserTemplate with
    member x.Usage =
      match x with
      | Version ->
        "Prints version information."
      | Verbose ->
        "Enables verbose logging while running Rutta. Useful for debugging your setup."
      | Proxy _ ->
        "Rutta in Proxy mode, does transparent forwarding of data between e.g. two subnets."
      | Router _ ->
        "Rutta in Router mode, starts N listeners, each with a *codec* and *mode* and *endpoint/binding* as its input."
      | Shipper _ ->
        "Rutta in shipper mode, sources metrics and sends it on to a Router or Proxy."
      | Health _ ->
        "Give Rutta health binding information. Without this flag, no health endpoint is started."

module Parsers =
  open System
  open System.Net
  open Logary
  open Logary.Configuration

  let binding (binding: string): IPEndPoint =
    match binding.Split([| ':' |]) with
    | [| nic; p |] ->
      IPEndPoint(IPAddress.Parse nic, int p)
    | _ ->
      failwithf "Invalid binding '%s'. Expected format '<ip>:<port>'." binding

  let targetConfig (targetUri: string): TargetConf =
    TargetConfig.create (Uri targetUri)