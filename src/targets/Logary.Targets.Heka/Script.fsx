// Learn more about F# at http://fsharp.net. See the 'F# Tutorial' project
// for more guidance on F# programming.
#r "../../../packages/NodaTime/lib/net35-Client/NodaTime.dll"
#r "../../../packages/protobuf-net/lib/net40/protobuf-net.dll"
#load "../Logary.Targets.Riemann/ProtoBufUtils.fs"
#load "Messages.fs"
#load "Client.fs"
#load "Targets_Heka.fs"
open Logary.Targets.Heka

