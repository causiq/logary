[<AutoOpen>]
module internal Logary.Prelude

open System.Reflection
let LogaryVersion =
  let ver = Assembly.GetExecutingAssembly().GetName().Version
  sprintf "%d.%d.%d" ver.Major ver.Minor ver.Build