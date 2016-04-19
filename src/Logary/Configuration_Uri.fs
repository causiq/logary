namespace Logary.Configuration

open Microsoft.FSharp.Reflection
open System
open System.Reflection
open System.Collections.Generic
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
  let private (|Info|NoInfo|) inp =
    match inp with
    | null
    | "" ->
      NoInfo

    | data ->
      match String.split ':' data with
      | [ u; p ] ->
        Info (u, p)

      | _ ->
        NoInfo

  let private conversion =
    let d = Dictionary<Type, _> ()
    [ typeof<uint16>, uint16 >> box
      typeof<uint32>, uint32 >> box
      typeof<uint64>, uint64 >> box
      typeof<int16>, int16 >> box
      typeof<int32>, int32 >> box
      typeof<int64>, int64 >> box
      typeof<string>, string >> box
      typeof<Uri>, (fun x -> Uri x) >> box
      typeof<Option<string>>, (function null -> None | x -> Some x) >> box
      typeof<float>, float >> box
      typeof<Single>, single >> box
      typeof<decimal>, decimal >> box
    ] |> List.iter d.Add
    d


  let private tryGetTryParse (typ : Type) =
    match typ.GetMethod("tryParse", BindingFlags.Static ||| BindingFlags.Public) with
    | null ->
      None

    | m ->
      Some (fun (s : string) ->
        m.Invoke(null, [| s |]) |> unbox : Choice<obj, string>)

  let internal convertTo typ (v : string) : obj =
    match conversion.TryGetValue typ with
    | false, _ ->
      match tryGetTryParse typ with
      | None ->
        Convert.ChangeType(v, typ) |> box

      | Some tryParse ->
        match tryParse v with
        | Choice1Of2 v ->
          box v

        | Choice2Of2 err ->
          Convert.ChangeType(v, typ) |> box

    | true, converter ->
      converter v
    
  

  let parseConfig<'recordType> (emptyValue : 'recordType) uriString =
    let argVals = 
      let uri = Uri uriString

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
        p.PropertyType, p.Name, p.GetValue emptyValue)

      |> Array.map (fun (typ, name, defaultValue) ->
        match qVals |> Map.tryFind name with
        | Some v ->
          v |> convertTo typ

        | None when String.equalsCaseInsensitve name "endpoint" 
                 && typ = typeof<Uri> ->
          let ub = UriBuilder uri
          let scheme' = if uri.Scheme.Contains("+") then uri.Scheme.Substring(uri.Scheme.IndexOf("+") + 1)
                        else uri.Scheme 
          ub.Scheme <- scheme'
          ub.UserName <- ""
          ub.Password <- ""
          box ub.Uri

        | None when String.equalsCaseInsensitve name "username" ->
          match uri.UserInfo with
          | Info (user, _) ->
            box (Some user)

          | NoInfo ->
            defaultValue

        | None when String.equalsCaseInsensitve name "password" ->
          match uri.UserInfo with
          | Info (_, pass) ->
            box (Some pass)

          | NoInfo ->
            defaultValue

        | None ->
          defaultValue)

    FSharpValue.MakeRecord(typeof<'recordType>, argVals) :?> 'recordType
