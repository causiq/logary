open Logary
open Logary.Message

type SomeInfo() =
  member x.PropA =
    45
  member x.PropB =
    raise (Exception ("Oh noes, no referential transparency here"))
with
  interface IFormattable with
    member x.ToString (format, provider) = "PropA is 45 and PropB raise exn"

let oneObj = SomeInfo ()

eventFormat (Info, "Hey {userName}, here is a default {info} and stringify {$info} and destructure {@info}", "You", oneObj)
  |> setSimpleName "Company.APIModule.calculateTotals"
  |> MessageWriter.levelDatetimeMessagePath.format

// alternative style:

event Info "user write some info"
  |> setField "userName" "You"
  |> setField "data" oneObj
  |> setSimpleName "Company.APIModule.calculateTotals"
  |> MessageWriter.levelDatetimeMessagePath.format