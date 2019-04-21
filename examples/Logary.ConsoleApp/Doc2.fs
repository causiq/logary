type SomeInfo() =
  member x.PropA =
    45
  member x.PropB =
    raise (Exception ("Oh noes, no referential transparency here"))
with
  interface IFormattable with
    member x.ToString (format, provider) = "PropA is 45 and PropB raise exn"

let oneObj = SomeInfo ()

Message.eventFormat (Info, "Hey {userName}, here is a default {info} and stringify {$info} and destructure {@info}", "You", oneObj)
|> Message.setSimpleName "somewhere.this.message.happened"
|> MessageWriter.levelDatetimeMessagePath.format

val it : string =
  "I 2018-01-26T09:08:21.6590074+00:00: Hey "You", here is a default "PropA is 45 and PropB raise exn" and stringify "FSI_0002+SomeInfo" and destructure SomeInfo { PropB: "The property (PropB) accessor threw an (TargetInvocationException): Oh noes, no referential transparency here", PropA: 45 } [somewhere.this.message.happened]
  fields:
    info =>
      SomeInfo {
        PropB => "The property (PropB) accessor threw an (TargetInvocationException): Oh noes, no referential transparency here"
        PropA => 45}
    userName => "You""

// or use this style:

Message.event Info "user write some info"
|> Message.setField "userName" "You"
|> Message.setField "data" oneObj
|> Message.setSimpleName "somewhere.this.message.happened"
|> MessageWriter.levelDatetimeMessagePath.format