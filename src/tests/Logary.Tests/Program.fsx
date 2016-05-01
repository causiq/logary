#I "bin/Release"
#r "Fuchu.dll"
open Fuchu
#r "Logary.dll"
open Logary
#r "FSharp.Core.dll"
#r "Hopac.Core.dll"
#r "Hopac.dll"
#r "NodaTime.dll"
#load "TestDSL.fs"
#load "Fac.fs"
#load "Registry.fs"

Tests.run Logary.Tests.Registry.registryMid