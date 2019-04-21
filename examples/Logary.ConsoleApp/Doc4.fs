type User =
  {
    id      : int
    name    : string
    created : DateTime
  }

let date20171111 =  DateTime.Parse("2017-11-11")
let foo () = { id = 999; name = "whatever"; created = date20171111}


let ex = exn "exception with data in it"
ex.Data.Add ("data 1 in exn", 1)
ex.Data.Add ("data foo in exn", foo ())
ex.Data.Add (foo(), foo())

Message.event Error "ouch"
|> Message.addExn ex
|> Message.addExn (exn "another exception")
|> Message.setSimpleName "somewhere.this.message.happened"
|> MessageWriter.levelDatetimeMessagePathNewLine.format

val it : string =
  "E 2018-01-26T09:30:37.7648557+00:00: ouch [somewhere.this.message.happened]
  others:
    _logary.errors =>
      -
        System.Exception {
          Message => "another exception"
          HResult => -2146233088}
      -
        System.Exception {
          Message => "exception with data in it"
          Data =>
            "data 1 in exn" => 1
            "data foo in exn" =>
              User {
                name => "whatever"
                id => 999
                created => 11/11/2017 12:00:00 AM}
            - key =>
                User {
                  name => "whatever"
                  id => 999
                  created => 11/11/2017 12:00:00 AM}
              value =>
                User {
                  name => "whatever"
                  id => 999
                  created => 11/11/2017 12:00:00 AM}
          HResult => -2146233088}
"