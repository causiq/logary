namespace Logary.Formatting

open Chiron
open JsonHelper
open Logary.Internals

module Json =

  let encode (data : obj) =
    data |> toJsonTypeShape Global.jsonEncoderRegistry

  let formatWith options (data : obj) =
    data |> encode |> Json.formatWith options
    
  let format (data : obj) =
    formatWith JsonFormattingOptions.Compact data