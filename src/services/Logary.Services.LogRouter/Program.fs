open System
open System.Reflection
open Topshelf
open Time
open Logary
//open Argu
[<assembly: AssemblyTitle("Logary LogRouter")>]
()

(* Specification:

  This software should be capable of shipping logs to some arbitrary target.

  v1: Hard-coded supported target types. Initially we'll just support InfluxDB.
  v2: More configurable target configuration that supports any target.

  This service can run in two modes; Shipper and Router. Servers can be
  implemented using Hopac's lightweight servers. Communication happens with
  ZMQ.

  ## The shipper
  Enables log shipping from hosts that are not directly connected to the router
  nor to InfluxDB.

  Should be spawnable on Unix. Should be service-installable on Windows using
  TopShelf.

  Shippers CONNECT PUSH sockets to the Router's PULL socket.
  See http://lists.zeromq.org/pipermail/zeromq-dev/2012-February/015917.html

  ./logrouter --shipper 

  ## The Router
  BINDs a PULL socket on a specified NIC/IP and PORT.
  Configures a single internal Target that pushes the received data.

  ## The proxy (only reference)
  BINDs a XSUB socket on a specified NIC/IP and PORT.
  BINDs a XPUB socket on a specified NIC/IP and PORT.

  By doing it this way, we can chain proxies together and create a mesh of log
  routers that forward their ingested log data up to some Router that then
  finally sends the data to some final Target.

  ## Serialisation
  Should use a fast (network/memory/CPU) serialisation format, preferrably one
  that supports zero copy.

  The two competitors are:
   - FsPickler @ https://nessos.github.io/FsPickler/tutorial.html#Picklers-and-Pickler-combinators
   - MessagePack @ https://github.com/msgpack/msgpack

*)

type Arguments =
  | Shipper of string
  | Router of string
  | Proxy of string
  (*
with
  interface IArgParserTemplate with
      member s.Usage =
          match s with
          | Working_Directory _ -> "specify a working directory."
          | Listener _ -> "specify a listener (hostname : port)."
          | Data _ -> "binary data in base64 encoding."
          | Port _ -> "specify a primary port."
          | Log_Level _ -> "set the log level."
          | Detach _ -> "detach daemon from console."*)

module Router =
  ()

module Shipper =
  ()

module Proxy =
  ()

[<EntryPoint>]
let main argv =
  let info : string -> unit = fun s -> Console.WriteLine(sprintf "%s logger/sample-service: %s" (DateTime.UtcNow.ToString("o")) s)
  let sleep (time : TimeSpan) = System.Threading.Thread.Sleep(time)

  let start hc =
    info "sample service starting"

    (s 30) |> HostControl.request_more_time hc
    sleep (s 1)

    Threading.ThreadPool.QueueUserWorkItem(fun cb ->
        sleep (s 3)
        info "requesting stop"
        hc |> HostControl.stop) |> ignore

    info "sample service started"
    true 
    
  let stop hc =
    info "sample service stopped"
    true
  
  Service.Default
  |> with_start start
  |> with_recovery (ServiceRecovery.Default |> restart (min 10))
  |> with_stop stop
  |> run