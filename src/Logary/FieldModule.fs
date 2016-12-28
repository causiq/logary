namespace Logary

open Logary.Internals.Aether

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>] // remove when field moved outside
module Field =

  let inline initWithUnit value units =
    failwith "TODO"

  let inline init (value : ^a) =
    failwith "TODO"

  module Optic =

    let value_ : Lens<Field, Value> =
      (fun (Field (value, mUnits)) -> value),
      fun v (Field (value, mUnits)) -> Field (v, mUnits)

    let units_ : Prism<Field, Units> =
      (fun (Field (_, mUnits)) -> mUnits),
      fun units (Field (value, _)) -> Field (value, Some units)
