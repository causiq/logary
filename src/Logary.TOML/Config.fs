namespace Logary.Configuration

open System

open Logary
open Logary.TOML

type Map'<'K, 'V> = System.Collections.Generic.IDictionary<'K, 'V>

/// A configuration key
type Key = string

/// The 'logary.service' key is missing
exception MissingService of Key

/// The key/config was giving the wrong type of data
exception WrongDataType of Key

module internal Impl =
  let ensure (dic : Map'<Key, obj>) (key : Key) (fFailure : Key -> exn) =
    match dic.TryGetValue key with
    | true, value ->
      try value :?> string
      with :? InvalidCastException ->
        raise (WrongDataType key)
    | false, _ -> raise (fFailure key)

[<AutoOpen>]
module TOMLConfig =
  open Impl
  open Logary.Configuration

  /// returns a configuration, or:
  /// throws MissingService if no service name is provided 'logary.service',
  /// or throws WrongDataType if the configuration doesn't typecheck.
  let confLogaryString str =
    let parsed = parse str
    let ensure = ensure parsed
    let service = ensure "logary.service" (fun key -> MissingService key)
    confLogary service