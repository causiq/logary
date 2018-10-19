namespace Logary.Internals

module Reflection =
  open System
  open Logary.Internals.TypeShape.Core

  /// Used internally to format the exception message to put in place of properties
  /// that could not be accessed without exceptions being thrown.
  ///
  /// Used by the below `propsFrom` as well as `JsonHelper`.
  let internal memberAccessExn (fieldOrPropName: string) (e: exn): string =
    sprintf "Accessing property '%s' threw '%s'.\n%O" fieldOrPropName (e.GetType().FullName) e

  /// Read the props from a plain old CLR object. Used a lot from C# with its
  /// anonymous objects.
  let propsFromFactory (valueType: Type): obj -> seq<string * obj> =
    match TypeShape.Create valueType with
    | Shape.Poco shape when shape.Properties.Length > 0 ->
      fun value ->
      seq {
        for prop in shape.Properties do
          match prop.MemberInfo with
          | :? Reflection.PropertyInfo as pi ->
            yield prop.Label,
              try
                pi.GetValue value
              with e ->
                box (memberAccessExn prop.Label e)
          | other ->
            //printfn "Other: %A" other
            ()
      }
    | _ ->
      fun _ -> Seq.empty

  let private prosFromFactoryWithMemorize = Logary.Internals.Cache.memoize propsFromFactory

  let propsFrom (value: obj): seq<string * obj> =
    match value with
    | null -> Seq.empty
    | _ -> prosFromFactoryWithMemorize (value.GetType()) value