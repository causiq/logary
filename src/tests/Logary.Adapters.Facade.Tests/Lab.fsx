#I "bin/Debug"
#r "Castle.Core.dll"
#r "Hopac.Core.dll"
#r "Hopac.dll"
#r "Logary.dll"
#r "Cibryy.dll"
#load "../../adapters/Logary.Adapters.Facade/Logary.Adapters.Facade.fs"

open Logary.Adapters.Facade

let loggerT = typeof<Cibryy.Logging.ILogger>

match loggerT with
| Reflection.CSharp (_, _, _) ->
  printfn "CS"
| _ ->
  printfn "FS"

Reflection.findModule (loggerT, "Global")
Reflection.findModule (loggerT, "Literals")
Reflection.readLiteral loggerT ("FacadeLanguage", "Nope")

open System
open System.IO
let bin = Path.Combine(__SOURCE_DIRECTORY__, "bin/Debug")
Directory.GetFiles(bin)

