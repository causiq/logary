module MyCompany.Sub.MyModule

open Logary

let logger = Logging.getCurrentLogger ()

let logInUser () =
  // do something
  Message.event Info "User logged in" |> Logger.logSimple logger
  // do more stuff