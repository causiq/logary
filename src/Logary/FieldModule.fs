namespace Logary

open Logary.Internals.Aether

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Field =

  let createUnit (value : 'a) (units : Units) =
    failwith "TODO: TypeShape"

  let create (value : 'a) =
    failwith "TODO: TypeShape"

  module Optic =

    let value_ : Lens<Field, Value> =
      (fun (Field (value, mUnits)) -> value),
      fun v (Field (value, mUnits)) -> Field (v, mUnits)

    let units_ : Prism<Field, Units> =
      (fun (Field (_, mUnits)) -> mUnits),
      fun units (Field (value, _)) -> Field (value, Some units)
