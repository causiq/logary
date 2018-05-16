namespace Logary

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Gauge =
  let format (Gauge (v, u)) =
    let gv, gu = Units.scale u (v.toFloat())
    sprintf "%.2f %s" gv gu
