#I "bin/Debug"
#r "FSharp.Actor.dll"
#r "NodaTime.dll"
#r "Newtonsoft.Json.dll"
#r "Logary.dll"
#r "protobuf-net.dll"
#load "../Logary.Targets.Riemann/ProtoBufUtils.fs"
#load "Messages.fs"
#load "Constants.fs"
#load "Types.fs"
#load "Client.fs"
#load "Targets_Heka.fs"
open Logary
open Logary.Heka
open Logary.Heka.Messages
open Logary.Heka.Client
open Logary.Configuration
open Logary.Targets
open Logary.Targets.Heka
open Logary.Metrics
open NodaTime

open System
open System.IO
open System.Net
open System.Net.Sockets

let conf = HekaConfig.Empty

let client = new TcpClient()
client.ConnectAsync(IPAddress.Parse("127.0.0.1"), 5566).Wait()
let stream = client.GetStream() :> Stream


let msg = Message(``type`` = "heka.logary",
                  timestamp = 1416840893000000000L,
                  uuid = Guid("11da185c-a111-4d4d-baf1-f8116dcc76ce").ToByteArray(),
                  logger = "",
                  severity = Nullable 7,
                  payload = "",
                  pid = Nullable 34746,
                  hostname = "coinduction",
                  fields = Collections.Generic.List ())

let ms = new MemoryStream()

match msg |> Encoder.encode conf ms with
| Choice1Of2 run -> run |> Async.RunSynchronously
| Choice2Of2 e -> failwithf "%A" e

printfn "uuid: %s" (BitConverter.ToString(msg.uuid).Replace("-", ""))

let arr = ms.ToArray()
printfn "wire: %s" (BitConverter.ToString(arr).Replace("-", ""))