namespace Logary.Services.Rutta

open Argu

type Args =
  | Push_To of pushConnectToSocket:string
  | Pub_To of pubBindSocket:string
  | Router of pullBindSocket:string
  | Router_Sub of subConnectSocket:string
  | Router_TCP of tcpBindSocket:string
  | Router_UDP of udpBindSocket:string
  | Router_HTTP of udpBindSocket:string
  | Router_Target of logaryTargetUri:string
  | Proxy of xsubConnectToSocket:string * xpubBindSocket:string
  | Health of ip:string * port:int
  | No_Health
with
  interface IArgParserTemplate with
    member s.Usage =
      match s with
      | Push_To _ -> "Runs Rutta in Shipper/PUSH mode (send Messages from a node to router)"
      | Pub_To _ -> "Runs Rutta in Shipper/PUB mode (send Messages from a node to proxy)"
      | Router _ -> "Runs Rutta in Router mode (PULL fan-in of Messages, forward to Target)."
      | Router_Sub _ -> "Runs Rutta in Router XSUB mode for PUB sockets to publish to."
      | Router_TCP _ -> "Runs Rutta in TCP STREAM server mode, that allows zmq to be used as a TCP socket. Send newline-separated JSON to this bound socket. See http://api.zeromq.org/4-0:zmq-socket#toc19"
      | Router_UDP _ -> "Runs Rutta in UDP server mode. Send newline-separated JSON datagrams to this socket."
      | Router_HTTP _ -> "Runs Rutta in HTTP server mode. Send JSON to /i/logary to have it converted to Messages."
      | Router_Target _ -> "Implied by --router. Specifies where the Router target should forward its data"
      | Proxy (_,_) -> "Runs Rutta in Proxy mode (XSUB/fan-in of Messages, forward to SUB sockets via XPUB). The first is the XSUB socket (in), the second is the XPUB socket (out)."
      | Health _ -> "Give Rutta health binding information"
      | No_Health _ -> "Don't bind a health endpoint on start."