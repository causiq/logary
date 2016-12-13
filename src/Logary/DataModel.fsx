#load "../../paket-files/xyncro/aether/src/Aether/Aether.fs"
#r "bin/Debug/NodaTime.dll"
#r "bin/Debug/FParsecCS.dll"
#r "bin/Debug/FParsec.dll"
#load "../../paket-files/xyncro/chiron/src/Chiron/Chiron.fs"
#load "../../paket-files/haf/YoLo/YoLo.fs"
#r "bin/Debug/Hopac.Core.dll"
#r "bin/Debug/Hopac.dll"
#load "Constants.fs"
#load "Internals.fs"
#load "LogLevel.fs"
#load "KnownLiterals.fs"
#load "MessageTemplates.fs"
#load "DataModel.fs"
open Logary

Units.scaleBy10 Seconds 10000. // x0.001, s => 1 ks
Units.scaleBy10 Seconds 1000. // x0.001, s => 1 ks
Units.scaleBy10 Seconds 100. // x1, s => 100 s
Units.scaleBy10 Seconds 10. // x1, s => 10 s
Units.scaleBy10 Seconds 1. // x1, s => 1 s
Units.scaleBy10 Seconds 0. // x1, s => 0 s
Units.scaleBy10 Seconds 0.1 // x1000, ms => 100 ms
Units.scaleBy10 Seconds 0.01 // x1000, ms => 10 ms
Units.scaleBy10 Seconds 0.001 // x1000, µs => 1 ms
Units.scaleBy10 Seconds 0.0001 // x1 000 000, µs => 100 µs
ceil (abs (log10 0.0009999999))
log10 0.0009
Units.scaleBy10 Seconds 0.00011 // x1 000 000, µs => 110 µs
Units.scaleBy10 Seconds 0.000001 // x1 000 000, ns => 1 µs
Units.scaleBy10 Seconds 0.00000099 // x1 000 000 000, ns => 990 ns
Units.scaleBy10 Seconds 0.000000001 // x1 000 000 000, ns => 1 ns