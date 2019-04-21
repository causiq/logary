Message.eventFormat (Info, "{userId} create an shopping list at {createdTime}", "9999", DateTime.Now )
|> Message.setContext "user name" ":)"
|> Message.setContext "shopping list" ["cat";"cat food";"books";"drinks"]
|> Message.setSimpleName "somewhere.this.message.happened"
|> MessageWriter.levelDatetimeMessagePath.format

val it : string =
  "I 2018-01-26T10:11:54.5221326+00:00: "9999" create an shopping list at 1/26/2018 6:11:54 PM [somewhere.this.message.happened]
  fields:
    userId => "9999"
    createdTime => 1/26/2018 6:11:54 PM
  others:
    user name => ":)"
    shopping list => ["cat", "cat food", "books", "drinks"]"