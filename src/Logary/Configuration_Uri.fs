namespace Logary.Configuration

open Microsoft.FSharp.Reflection
open System
open System.Reflection
open Logary

type internal Helper() =
  static member getDefaultGeneric<'T> () =
    Unchecked.defaultof<'T>

  static member getDefault (t : Type) : obj =
    typeof<Helper>
      .GetMethod("getDefaultGeneric", BindingFlags.NonPublic ||| BindingFlags.Static)
      .MakeGenericMethod(t)
      .Invoke(null, null)

module Uri =
  let parseConfig<'recordType> uriString =
  //  let parsedPort = 8086us
  //  let dbName = "dbName"
  
    let endpoint = "{scheme}user:pass@host:8086/write"

    let argVals = 
      let uri = Uri uriString

      let convertTo t (v:string) : obj=
        match t with
        | t when t = typeof<string> ->
          box v

        | t when t = typeof<int32> ->
          box (int v)

      let qVals = 
        uri.Query
        |> String.trimc '?'
        |> String.split '&'
        |> List.map (String.split '=' >> function
          | [ k; v] -> k, v
          | other -> failwithf "unexpected %A" other)
        |> Map.ofList

      FSharpType.GetRecordFields(typeof<'recordType>)
      |> Array.map (fun p ->
        p.PropertyType, p.Name)

      |> Array.map (fun (t,n) ->
        match qVals |> Map.tryFind n with
        | Some v ->
          v |> convertTo t

        | None ->
          Helper.getDefault t)

    FSharpValue.MakeRecord(typeof<'recordType>, argVals) :?> 'recordType
