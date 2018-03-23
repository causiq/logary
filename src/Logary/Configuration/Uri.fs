namespace Logary.Configuration

open Microsoft.FSharp.Reflection
open System
open System.Reflection
open System.Collections.Generic
open Logary

type internal Helper() =
  static member getDefaultGeneric<'T> () =
    Unchecked.defaultof<'T>

  static member getDefault (t: Type): obj =
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


  let private tryGetTryParse (typ: Type) =
    match typ.GetMethod("tryParse", BindingFlags.Static ||| BindingFlags.Public) with
    | null ->
      None

    | m ->
      Some (fun (s: string) ->
        m.Invoke(null, [| s |]) |> unbox: Choice<obj, string>)

  let convertTo typ (v: string): obj =
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

  let parseConfig (recordType: Type) (emptyValue: obj) (uri: Uri) =
    let argVals =
      let qVals =
        uri.Query
        |> String.trimc '?'
        |> String.split '&'
        |> List.filter (String.IsNullOrWhiteSpace >> not)
        |> List.map (String.split '=' >> function
          | [ k; v] -> k, v
          | other -> failwithf "Unexpected %A" other)
        |> Map.ofList

      FSharpType.GetRecordFields recordType
      |> Array.map (fun p ->
        p.PropertyType, p.Name, p.GetValue emptyValue)

      |> Array.map (fun (typ, name, defaultValue) ->
        match qVals |> Map.tryFind name with
        | Some v ->
          v |> convertTo typ

        | None when String.equalsCaseInsensitive name "endpoint"
                 && typ = typeof<Uri> ->
          let ub = UriBuilder uri
          let scheme' = if uri.Scheme.Contains("+") then uri.Scheme.Substring(uri.Scheme.IndexOf("+") + 1)
                        else uri.Scheme
          ub.Scheme <- scheme'
          ub.UserName <- ""
          ub.Password <- ""
          box ub.Uri

        | None when String.equalsCaseInsensitive name "username" ->
          match uri.UserInfo with
          | Info (user, _) ->
            box (Some user)

          | NoInfo ->
            defaultValue

        | None when String.equalsCaseInsensitive name "password" ->
          match uri.UserInfo with
          | Info (_, pass) ->
            box (Some pass)

          | NoInfo ->
            defaultValue

        | None ->
          defaultValue)

    FSharpValue.MakeRecord(recordType, argVals)

  let parseConfigString recordType emptyValue uriString =
    parseConfig recordType emptyValue (Uri uriString)

  let parseConfigT<'recordType> (emptyValue: 'recordType) uri =
    parseConfig typeof<'recordType> (box emptyValue) uri
    :?> 'recordType

  let parseConfigTString<'recordType> emptyValue uriString =
    parseConfigT<'recordType> emptyValue (Uri uriString)

module TargetConfig =
  let modu name = sprintf "Logary.Targets.%s" name
  let asm name = sprintf "Logary.Targets.%s" name
  let conf name = sprintf "%sConf" name

  let moduleNameConfigName modu asm conf =
    sprintf "%s, %s" modu asm,
    sprintf "%s+%s, %s" modu conf asm

  let moduleNameConfigNameAsm name =
    moduleNameConfigName (modu name) (asm name) (conf name)

  type DynamicConfig =
    { configType: Type
      moduleName: string
      moduleType: Type
    }
    /// Creates the default target configuration particular to the target.
    member x.getDefault () =
      if isNull x.moduleType then
        failwithf "Module '%s' did not resolve. Do you have its DLL next to rutta.exe?" x.moduleName

      let defaultEmpty = x.moduleType.GetProperty("empty")
      if isNull defaultEmpty then
        failwithf "Module '%s' did not have a default config value named 'empty'." x.moduleName

      defaultEmpty.GetValue(null)

    /// Creates the final Logary TargetConf value that logary uses to build the target.
    member x.createTargetConf (conf: obj) (name: string): TargetConf =
      if isNull x.moduleType then
        failwithf "Module '%s' did not have 'empty' conf-value. This should be fixed in the target's code."
                  x.moduleName

      let createMethod = x.moduleType.GetMethod("Create")
      if isNull createMethod then
        failwithf "Module '%s' did not have 'create' \"(name: string) -> (conf: 'conf) -> TargetConf\" function. This should be fixed in the target's code (with [<CompiledName \"Create\">] on itself)."
                  x.moduleName

      printfn "Invoking create on '%O'" createMethod
      createMethod.Invoke(null, [| conf; name |])
      :?> TargetConf

    static member create configType moduleName moduleType =
      //printfn "Create DynamicConfig with (configType=%A, moduleName=%A, moduleType=%A)" configType moduleName moduleType
      { configType = configType
        moduleName = moduleName
        moduleType = moduleType }

  let schemeToConfAndDefault =
    [ "influxdb",    moduleNameConfigNameAsm "InfluxDb"
      "stackdriver", moduleNameConfigNameAsm "Stackdriver"
      "console",     moduleNameConfigName (modu "LiterateConsole") "Logary" (conf "LiterateConsole")
    ]
    |> List.map (fun (scheme, (moduleName, configName)) ->
      let confType = Type.GetType configName
      let moduleType = Type.GetType moduleName
      scheme, DynamicConfig.create confType moduleName moduleType)
    |> Map

  let create (targetUri: Uri): TargetConf =
    printfn "Creating a new target from URI: '%O'" targetUri
    let scheme = targetUri.Scheme.ToLowerInvariant()
    match schemeToConfAndDefault |> Map.tryFind scheme with
    | None ->
      failwithf "Rutta has not get got support for '%s' targets" scheme
    | Some dynamicConfig ->
      let configDefault = dynamicConfig.getDefault ()
      Uri.parseConfig dynamicConfig.configType configDefault targetUri
      |> fun config -> dynamicConfig.createTargetConf config scheme
