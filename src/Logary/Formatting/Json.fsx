#I "../bin/Debug/net461"
#r "FParsec"
#r "Hopac.Core"
#r "Hopac"
#r "Logary"
open System
open System.Reflection
open Logary.Internals.Chiron
open Logary.Internals.TypeShape.Core
open Logary.Formatting.JsonHelper
module E = Json.Encode
toJson() "a"
toJson() (box "a")
toJson() (box [ (box "a") ])
toJson() (box [ "a" ])
toJson() []
toJson() null
toJson() ()
toJson() (obj())
toJson() (Map [ "a", String "b"])
