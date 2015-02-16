#I "bin/Release"
#r "Fuchu.dll"
open Fuchu
#r "Logary.dll"
open Logary
#r "FSharp.Core.dll"

#load "StubTcp.fs"
#load "TestDSL.fs"

Tests.run