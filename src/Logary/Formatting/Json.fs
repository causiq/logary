namespace Logary.Formatting

open Chiron
open JsonHelper
open Logary.Internals

/// See JsonHelper.fs
module Json =

  let encode (data: obj): Json =
    data |> toJsonTypeShape Global.jsonEncoderRegistry

  let formatWith options (data: obj): string =
    encode data
    |> Json.formatWith options

  let format (data: obj) =
    formatWith JsonFormattingOptions.Compact data