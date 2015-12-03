module Program

open System
open System.Data
open System.Data.SqlClient
open System.Net

open NodaTime

open Topshelf

open Logary
open Logary.Targets
open Logary.Configuration

open Logary.Metrics

open Nessor.Argu

open SQLServerIOInfo

exception RiemannServerNotFound of System.Net.Sockets.SocketException * string
with
  override x.Message =
    x.ToString()
  override x.ToString() =
    sprintf "Riemann Server '%s' was not resolvable to IP in DNS:\n%O"
      x.Data1 x.Data0

type Arguments =
  | Drive_Latency of DriveName
  | Database_Latency of DatabaseName
  | File_Latency of FullyQualifiedPath
  | Sampling_Period of int64
  | [<Mandatory>] Connection_String of string
  | Riemann_Host of string
  | Riemann_Port of int
with
  interface IArgParserTemplate with
    member x.Usage =
      match x with
      | Drive_Latency _ ->
        "E.g. 'C:' or 'D:' - usually all things on the drive have similar \
         performance metrics as it's the underlying device that sets the \
         constraints. Do not include the backslash in this name."
      | Database_Latency _ ->
        "E.g. 'MyDatabase'; the database inside SQL server that you want to \
         probe"
      | File_Latency _ ->
        "A single file is the lowest qualifier that gives unique latency results"
      | Sampling_Period _ ->
        "A sampling period is the interval time in milliseconds between calls \
         to the database"
      | Connection_String _ ->
        "The database to connect to"
      | Riemann_Host _ ->
        "The hostname/ip of riemann"
      | Riemann_Port _ ->
        "The port to connect to riemann on"

let openConn connStr : IDbConnection =
  let c = new SqlConnection(connStr)
  c.Open()
  upcast c

let tryLookup (name : string) =
  try
    Choice1Of2 (Dns.GetHostEntry(name).AddressList.[0])
  with :? System.Net.Sockets.SocketException as e ->
    Choice2Of2 e

let parseIP str =
  match IPAddress.TryParse str with
  | false, _ ->
    match tryLookup str with
    | Choice1Of2 ip -> ip
    | Choice2Of2 e  -> raise (RiemannServerNotFound (e, str))
  | true, ip -> ip

let parse args =
  let parser  = Argu.Create<Arguments>()
  let parse   = parser.Parse args
  let drives  = parse.PostProcessResults(<@ Drive_Latency @>, Drive)
  let files   = parse.PostProcessResults(<@ File_Latency @>, SingleFile)
  let period  = parse.TryPostProcessResult(<@ Sampling_Period @>,
                                           Duration.FromMilliseconds)
                |> Option.fold (fun _ t -> t) (Duration.FromMilliseconds(1000L))
  let connStr = parse.GetResult <@ Connection_String @>
  let conf    = { SQLServerIOInfo.empty with
                    latencyTargets = drives @ files
                    openConn       = fun () -> openConn connStr }
  let riemann = parse.PostProcessResult(<@ Riemann_Host @>, parseIP)
  let port    = parse.GetResult(<@ Riemann_Port@>, defaultValue = 5555)

  period, conf, IPEndPoint(riemann, port)

[<EntryPoint>]
let main args =
  let period, appconf, riemann = parse args
  let logary = ref None
  with_topshelf <| fun conf ->
    run_as_network_service conf
    naming_from_this_asm conf
    with_recovery conf (restart (TimeSpan.FromSeconds 20.))
    service conf <| fun _ ->
      { new ServiceControl with
          member x.Start hc =
            logary :=
              withLogary' "Logary.SQLServerHealth" (
                withTargets [
                  Console.create Console.empty "console"
                  Riemann.create (Riemann.RiemannConf.Create(endpoint = riemann))
                    "riemann"
                ] >>
                withRules [
                  Rule.createForTarget "console"
                  Rule.createForTarget "riemann"
                ] >>
                withMetrics (Duration.FromSeconds 10L) [
                  SQLServerIOInfo.create appconf "sql_server_io_info" period
                ])
              |> Some
            true
          member x.Stop hc =
            match !logary with
            | None -> ()
            | Some l -> l.Dispose()
            true
      }
