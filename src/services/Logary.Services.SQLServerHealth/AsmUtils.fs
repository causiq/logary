module Logary.AsmUtils

open System.IO
open System.Reflection

type ResourceName = string

let readResource (name : ResourceName) =
  let assembly = Assembly.GetExecutingAssembly()
  let resource = assembly.GetManifestResourceStream name
  let sr = new StreamReader(resource)
  sr.ReadToEnd()