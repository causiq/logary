namespace Logary.Services.Rutta

open Logary.Ingestion

#nowarn "49"
// ignore "Uppercase variable identifiers should not generally be used in patterns, and may indicate a misspelt pattern name."

open Argu
open Logary.Configuration

open System.Reflection
[<assembly: AssemblyTitle("Logary Rutta – a router/proxy/shipper for Windows and Unix")>]
()


type RuttaMode =
  /// ØMQ PULL socket. Does a PULL socket bind call.
  | Pull
  /// ØMQ SUB socket. Does a SUB connect call.
  | Sub
  /// ØMQ STREAM socket: a basic TCP listener
  | TCP
  /// UDP listener
  | UDP
  /// HTTP listener
  | HTTP
  member x.isWithØMQ =
    match x with | Pull | Sub | TCP -> true | _ -> false
  override x.ToString() =
    match x with
    | Pull -> "pull"
    | Sub -> "sub"
    | TCP -> "tcp"
    | UDP -> "udp"
    | HTTP -> "http"
  member x.asScheme =
    Scheme (x.ToString())

[<RequireQualifiedAccess>]
type RuttaCodec =
  /// Uses the newline-separated JSON codec
  | Json
  /// Takes the newline-separates message and create a simple message from it
  | Plain
  /// Log4J-XML input; each event should be newline-separated.
  | Log4jXML
  /// Used for Rutta-to-Rutta communication with the ZMQ socket types
  | Binary

[<RequireSubcommand>]
type ProxySubCommand =
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

  /// Whether Argu should go through every union case; in RELEASE mode it does not.
  let checkStructure =
  #if RELEASE
    false
  #else
    true
  #endif

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
    let folder (sb: StringBuilder) (u: RuttaMode) =
      let mInfo, _ = FSharpValue.GetUnionFields(u, typeof<RuttaMode>)
      let mdesc = describeMode u
      sb.AppendFormat("- {0}: {1}{2}", mInfo.Name.ToLowerInvariant(), mdesc, Environment.NewLine)
    [ Pull; Sub; TCP; UDP; HTTP ]
    |> Seq.fold folder (StringBuilder())
    |> sprintf "%O"

  let describeCodec = function
    | RuttaCodec.Json -> "Uses the JSON codec; framing (what separates messages) is up to the listener mode."
    | RuttaCodec.Plain -> "Takes the newline-separates message and create a simple message from it"
    | RuttaCodec.Binary -> "Used for Rutta-to-Rutta communication with the ZMQ socket types"
    | RuttaCodec.Log4jXML -> "Takes Log4j XML event, separated with newlines, as input"

  let describeCodecs () =
    let folder (sb: StringBuilder) (u: RuttaCodec) =
      let mInfo, _ = FSharpValue.GetUnionFields(u, typeof<RuttaCodec>)
      let mdesc = describeCodec u
      sb.AppendFormat(" - {0}: {1}{2}", mInfo.Name.ToLowerInvariant(), mdesc, Environment.NewLine)
    [ RuttaCodec.Json; RuttaCodec.Plain; RuttaCodec.Binary; RuttaCodec.Log4jXML ]
    |> Seq.fold folder (StringBuilder())
    |> sprintf "%O"

[<RequireQualifiedAccess; RequireSubcommand>]
type RouterSubCommand =
  | [<Mandatory>] Listener of routerMode:RuttaMode * bindingOrEndpoints:string * codec:RuttaCodec
  | [<Mandatory>] Target of targetUri:string
  | Disable_CORS

  interface IArgParserTemplate with
    member x.Usage =
      match x with
      | Listener _ ->
        System.String.Concat [
          "A listener is a triple of <router mode, binding, codec>. You can specify many of these:\n"
          "## Router modes:\n"
          Help.describeModes ()
          "## Binding(s)\n"
          "E.g. \"127.0.0.1:20001\" or \"0.0.0.0:9090,0.0.0.0:9443\" or \"[::]:8080\" or \"[::]:8080,[1234::1]:9090\"\n"
          "\n"
          "## Codecs:\n"
          Help.describeCodecs ()
        ]

      | Disable_CORS ->
        "Disables CORS, but your log ingestion is probably still open to people doing a `curl`."

      | Target _ ->
        let available =
          TargetConfig.schemeToConfAndDefault
          |> Seq.map (fun (KeyValue (k, _)) -> sprintf "- %s://" k)
          |> String.concat "\n"
        sprintf "Specifies a targets to ship to. You can specify many of these. Available targets:\n%s" available

[<RequireSubcommand>]
type ShipperSubCommand =
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
  | [<CliPrefix(CliPrefix.None)>] Proxy of args:ParseResults<ProxySubCommand>
  | [<CliPrefix(CliPrefix.None)>] Router of args:ParseResults<RouterSubCommand>
  | [<CliPrefix(CliPrefix.None)>] Shipper of args:ParseResults<ShipperSubCommand>
  | [<AltCommandLine "-h"; Inherit>] Health of binding:string
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
  open Logary

  let bindingString (binding: string): Binding =
    let portIndex = binding.LastIndexOf(':')
    let nic, port = binding.[0..portIndex-1], binding.[portIndex+1..]
    Binding.create("tcp", nic, uint16 port)

  let listener (mode: RuttaMode, nicAndPorts: string, codec: RuttaCodec): RuttaMode * BindingList * RuttaCodec =
    let parseNICAndPort (nicAndPort: string) =
      let portIndex = nicAndPort.LastIndexOf(':')
      NIC nicAndPort.[0..portIndex-1],
      Port (uint16 nicAndPort.[portIndex+1..])

    let parseNICAndPorts (nicAndPorts: string) =
      nicAndPorts.Split(',', StringSplitOptions.RemoveEmptyEntries)
        |> Array.map (fun s -> s.Trim())
        |> Array.map parseNICAndPort

    let bindings =
      parseNICAndPorts nicAndPorts
        |> Array.map (fun (nic, port) -> Binding (mode.asScheme, nic, port))
        |> BindingList.create

    mode, bindings, codec

  let targetConfig (targetUri: string): TargetConf =
    TargetConfig.create (Uri targetUri)