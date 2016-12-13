#I "bin/Debug"
#r "Expecto.dll"
open Expecto
#r "Logary.dll"
open Logary
open Logary.Configuration
open Logary.Targets
//#r "FSharp.Core.dll"
#r "Hopac.Core.dll"
#r "Hopac.dll"
#r "NodaTime.dll"
open System

let currDir = Environment.CurrentDirectory
let logDir = System.IO.Path.Combine(currDir, "logs")

printfn "Logging in %s" logDir
open Hopac
open NodaTime
open Logary.Targets.File

let fileConf =
  { File.FileConf.create logDir (Naming ("{service}-{host}-{datetime}", "log")) }

let logary =
  withLogaryManager "Program-fsx" (
    withTargets [
      LiterateConsole.create LiterateConsole.empty "fatal"
      File.create fileConf "file"
    ] >>
    withRules [
      Rule.createForTarget "fatal"
      Rule.createForTarget "file"
    ] >>
    withInternalTargets Debug [
      LiterateConsole.create LiterateConsole.empty "console"
    ]
  )
  |> run

let logger =
  logary.getLogger (PointName [| "Logary"; "Samples"; "main" |])

let acks = ResizeArray<_>(10000)
job {
  for i in 1 .. 10000 do
    let! ack =
      Message.event Logary.LogLevel.Info "Event {number}"
      |> Message.setField "number" i
      |> Logger.logWithAck logger
    //do! ack
    acks.Add ack

  // wait for them all!
  do! Job.conIgnore acks
} |> start

logary.Dispose()



// use for testing specific tests
//#load "TestDSL.fs"
//#load "Fac.fs"
//#load "Registry.fs"
//Tests.run Logary.Tests.Registry.registryMid

