#r "bin/Release/Fuchu.dll"
open Fuchu
#r "bin/Release/Intelliplan.Logary.dll"
open Logary
#r "bin/Release/FSharp.Core.dll"
#r "bin/Release"

#load "StubTcp.fs"
#load "TestDSL.fs"
#load "Tests.fs"



Tests.run